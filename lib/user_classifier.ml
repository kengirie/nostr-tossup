open Eio.Std

let fetch_kind1_events
    ?until
    ~label
    ~limit
    ~env
    ~clock
    ~ephemeral_pool
    ~relays
    ~timeout
    pubkey_hex =
  match relays with
  | [] -> []
  | _ ->
    let filter_json =
      Nostr_subscribe.Filter.(create ~authors:[pubkey_hex] ~kinds:[1] ?until ~limit () |> to_yojson)
    in
    Nostr_ephemeral_pool.query
      ephemeral_pool
      ~env
      ~clock
      ~relays
      ~filters:[filter_json]
      ~timeout
      ~label:label
      ()

let check_kind1_kana ~sw ~stdenv ~ephemeral_pool ~env ~clock ~relays ?uri npub_hex npub =
  let kind1_events =
    fetch_kind1_events
      ~label:"kind1-kana"
      ~limit:5
      ~env
      ~clock
      ~ephemeral_pool
      ~relays
      ~timeout:30.
      npub_hex
  in
  traceln "kind1 kana fetch for %s: %d events" npub (List.length kind1_events);
  match kind1_events with
  | [] ->
    traceln "No kind1 events found for %s, keeping existing_user=0" npub;
    Ok ()
  | _ ->
    let has_kana = List.exists (fun event ->
      match Yojson.Safe.Util.member "content" event with
      | `String content -> Text_utils.contains_kana content
      | _ -> false
    ) kind1_events in
    if not has_kana then (
      traceln "No kana found in kind1 events for %s, marking as existing_user=1" npub;
      Database.with_connection ?uri ~sw ~stdenv @@ fun conn ->
      match User_repository.update_existing_user conn ~npub ~existing_user:1 with
      | Ok () ->
        traceln "Updated %s existing_user=1 (no kana in kind1)" npub;
        Ok ()
      | Error err -> Error err
    ) else (
      traceln "Kana found in kind1 events for %s, keeping existing_user=0" npub;
      Ok ()
    )

let check_old_kind1 ~sw ~stdenv ~ephemeral_pool ~env ~clock ~relays ?uri npub_hex npub =
  let six_months_ago = Int64.to_int (Int64.sub (Int64.of_float (Unix.time ())) (Int64.of_int (6 * 30 * 24 * 3600))) in
  let kind1_events =
    fetch_kind1_events
      ~until:six_months_ago
      ~label:"kind1-old"
      ~limit:1
      ~env
      ~clock
      ~ephemeral_pool
      ~relays
      ~timeout:30.
      npub_hex
  in
  traceln "kind1 old fetch for %s: %d events" npub (List.length kind1_events);
  match kind1_events with
  | [] ->
    traceln "No old kind1 events found for %s, keeping existing_user=0" npub;
    Ok ()
  | _ ->
    traceln "Old kind1 events found for %s, marking as existing_user=1" npub;
    Database.with_connection ?uri ~sw ~stdenv @@ fun conn ->
    match User_repository.update_existing_user conn ~npub ~existing_user:1 with
    | Ok () ->
      traceln "Updated %s existing_user=1 (old kind1 found)" npub;
      Ok ()
    | Error err -> Error err

let contains_substring needle haystack =
  let needle_len = String.length needle in
  let haystack_len = String.length haystack in
  if needle_len = 0 then true
  else if needle_len > haystack_len then false
  else
    let rec loop i =
      if i + needle_len > haystack_len then false
      else if String.sub haystack i needle_len = needle then true
      else loop (i + 1)
    in
    loop 0

let retry_db_operation ~clock f max_retries delay =
  let rec loop retries =
    match f () with
    | Ok result -> Ok result
    | Error err when retries > 0 ->
      let err_str = Caqti_error.show err in
      if contains_substring "database is locked" err_str then (
        Eio.Time.sleep clock delay;
        loop (retries - 1)
      ) else Error err
    | Error err -> Error err
  in
  loop max_retries

let start ~sw ~clock ~stdenv ~ephemeral_pool ~env ?uri ?(relays = Config.periodic_relays) ?(interval = 300.) () =
  let rec kana_check_loop () =
    (match retry_db_operation ~clock (fun () ->
       Database.with_connection ?uri ~sw ~stdenv @@ fun conn ->
       User_repository.next_new_user conn) 3 0.5 with
     | Ok (Some npub) ->
       (match Nostr_utils.npub_to_hex npub with
        | Ok pubkey_hex ->
          (match check_kind1_kana ~sw ~stdenv ~ephemeral_pool ~env ~clock ~relays ?uri pubkey_hex npub with
           | Ok () -> ()
           | Error err ->
             traceln "Failed to check kind1 kana for %s: %a" npub Caqti_error.pp err)
        | Error msg ->
          traceln "Failed to decode npub %s for kana check: %s" npub msg)
     | Ok None -> ()
     | Error err -> traceln "kana check fetch failed: %a" Caqti_error.pp err);
    Eio.Time.sleep clock interval;
    kana_check_loop ()
  in

  let rec old_kind1_check_loop () =
    (match retry_db_operation ~clock (fun () ->
       Database.with_connection ?uri ~sw ~stdenv @@ fun conn ->
       User_repository.next_new_user conn) 3 0.5 with
     | Ok (Some npub) ->
       (match Nostr_utils.npub_to_hex npub with
        | Ok pubkey_hex ->
          (match check_old_kind1 ~sw ~stdenv ~ephemeral_pool ~env ~clock ~relays ?uri pubkey_hex npub with
            | Ok () -> ()
            | Error err ->
              traceln "Failed to check old kind1 for %s: %a" npub Caqti_error.pp err)
        | Error msg ->
          traceln "Failed to decode npub %s for old kind1 check: %s" npub msg)
     | Ok None -> ()
     | Error err -> traceln "old kind1 check fetch failed: %a" Caqti_error.pp err);
    Eio.Time.sleep clock interval;
    old_kind1_check_loop ()
  in

  Fiber.fork ~sw (fun () -> kana_check_loop ());
  Fiber.fork ~sw (fun () ->
    Eio.Time.sleep clock 30.;  (* Wait 15 seconds before starting *)
    old_kind1_check_loop ())
