open Eio.Std

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

let content_indicates_bot event =
  match Yojson.Safe.Util.member "content" event with
  | `String content ->
    let lower = String.lowercase_ascii content in
    contains_substring "bot" lower || contains_substring "rss" lower || contains_substring "ボット" content
  | _ -> false

let tag_string_indicates_bot s =
  let lower = String.lowercase_ascii s in
  lower = "proxy" || contains_substring "mostr" lower

let tags_indicate_bot event =
  match Yojson.Safe.Util.member "tags" event with
  | `List tags ->
    List.exists
      (function
        | `List entries ->
          List.exists
            (function
              | `String s -> tag_string_indicates_bot s
              | _ -> false)
            entries
        | _ -> false)
      tags
  | _ -> false

let event_indicates_bot event =
  content_indicates_bot event || tags_indicate_bot event

let fetch_kind0_event ~env ~clock ~ephemeral_pool ~relays ~timeout pubkey_hex =
  match relays with
  | [] -> None
  | _ ->
    let filter_json =
      Nostr_subscribe.Filter.(create ~authors:[pubkey_hex] ~kinds:[0] ~limit:1 () |> to_yojson)
    in
    let events =
      Nostr_ephemeral_pool.query
        ephemeral_pool
        ~env
        ~clock
        ~relays
        ~filters:[filter_json]
        ~timeout
        ~label:"kind0-botcheck"
        ()
    in
    match events with
    | event :: _ -> Some event
    | [] -> None

let classify_npub ~sw ~stdenv ~ephemeral_pool ~env ~clock ~relays ?uri npub_hex npub =
  match fetch_kind0_event ~env ~clock ~ephemeral_pool ~relays ~timeout:30. npub_hex with
  | None ->
    traceln "No kind0 event found for %s, marking as existing_user=1" npub;
    Database.with_connection ?uri ~sw ~stdenv @@ fun conn ->
    (match User_repository.update_existing_user conn ~npub ~existing_user:1 with
     | Ok () ->
       traceln "Updated %s existing_user=1" npub;
       Ok ()
     | Error err -> Error err)
  | Some event ->
    let is_bot = if event_indicates_bot event then 1 else 0 in
    Database.with_connection ?uri ~sw ~stdenv @@ fun conn ->
    match User_repository.update_is_bot conn ~npub ~is_bot with
    | Ok () ->
      traceln "Updated %s is_bot=%d" npub is_bot;
      Ok ()
    | Error err -> Error err


let start
    ~sw
    ~clock
    ~stdenv
    ~ephemeral_pool
    ~env
    ?uri
    ?(relays = Config.periodic_relays)
    ?(interval = 600.)
    ?(initial_delay = 0.)
    () =
  let rec loop () =
    (match Database.with_connection ?uri ~sw ~stdenv @@ fun conn ->
       User_repository.next_unclassified_user conn with
     | Ok (Some npub) ->
       (match Nostr_utils.npub_to_hex npub with
        | Ok pubkey_hex ->
          (match classify_npub ~sw ~stdenv ~ephemeral_pool ~env ~clock ~relays ?uri pubkey_hex npub with
           | Ok () -> ()
           | Error err ->
             traceln "Failed to update is_bot for %s: %a" npub Caqti_error.pp err)
        | Error msg ->
          traceln "Failed to decode npub %s: %s" npub msg;
          (match Database.with_connection ?uri ~sw ~stdenv @@ fun conn ->
             User_repository.update_is_bot conn ~npub ~is_bot:1 with
           | Ok () -> ()
           | Error err ->
             traceln "Failed to mark invalid npub %s: %a" npub Caqti_error.pp err))
     | Ok None -> ()
     | Error err -> traceln "is_bot fetch failed: %a" Caqti_error.pp err);
    Eio.Time.sleep clock interval;
    loop ()
  in
  Fiber.fork ~sw (fun () ->
    if initial_delay > 0. then Eio.Time.sleep clock initial_delay;
    loop ())
