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
    contains_substring "bot" lower || contains_substring "rss" lower
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

let fetch_kind0_event ~clock ~subscriber ~relays ~timeout pubkey_hex =
  match relays with
  | [] -> None
  | _ ->
    let filter_json =
      Nostr_subscribe.Filter.(create ~authors:[pubkey_hex] ~kinds:[0] ~limit:1 () |> to_yojson)
    in
    let promise, resolver = Promise.create () in
    let resolved = ref false in
    let resolve value =
      if not !resolved then (
        resolved := true;
        Promise.resolve resolver value)
    in
    let subscription_ref = ref None in
    let on_event event =
      resolve (`Event event);
      (match !subscription_ref with
       | Some sub ->
         subscription_ref := None;
         Nostr_subscribe.close_subscription_handle subscriber sub
       | None -> ())
    in
    let on_eose () = resolve `Eose in
    let on_close reasons = resolve (`Closed reasons) in
    let outcome =
      try
        let subscription =
          Nostr_subscribe.subscribe_simple
            subscriber
            ~relays
            ~filter:filter_json
            ~on_event
            ~on_eose
            ~on_close
            ()
        in
        subscription_ref := Some subscription;
        (match Eio.Time.with_timeout clock timeout (fun () -> Ok (Promise.await promise)) with
         | Ok value -> Some value
         | Error `Timeout -> None)
      with
      | exn ->
        traceln "Kind0 fetch error for %s: %s" pubkey_hex (Printexc.to_string exn);
        None
    in
    (match !subscription_ref with
     | Some sub ->
       subscription_ref := None;
       Nostr_subscribe.close_subscription_handle subscriber sub
     | None -> ());
    match outcome with
    | Some (`Event event) -> Some event
    | _ -> None

let classify_npub ~sw ~stdenv ~subscriber ~clock ~relays ?uri npub_hex npub =
  match fetch_kind0_event ~clock ~subscriber ~relays ~timeout:20. npub_hex with
  | None ->
    traceln "No kind0 event found for %s" npub;
    Ok ()
  | Some event ->
    let is_bot = if event_indicates_bot event then 1 else 0 in
    Database.with_connection ?uri ~sw ~stdenv @@ fun conn ->
    match User_repository.update_is_bot conn ~npub ~is_bot with
    | Ok () ->
      traceln "Updated %s is_bot=%d" npub is_bot;
      Ok ()
    | Error err -> Error err

let start ~sw ~clock ~stdenv ~subscriber ?uri ?(relays = Config.subscribe_relays) ?(interval = 300.) () =
  let rec loop () =
    (match Database.with_connection ?uri ~sw ~stdenv @@ fun conn ->
       User_repository.next_unclassified_user conn with
     | Ok (Some npub) ->
       (match Nostr_utils.npub_to_hex npub with
        | Ok pubkey_hex ->
          (match classify_npub ~sw ~stdenv ~subscriber ~clock ~relays ?uri pubkey_hex npub with
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
  Fiber.fork ~sw (fun () -> loop ())
