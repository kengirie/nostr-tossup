open Eio.Std

type publish_fn = relays:string list -> Nostr_event.signed_event -> Nostr_publish.result list

type t = {
  publish : publish_fn;
  mutable last_events : Nostr_event.signed_event list;
  mutex : Eio.Mutex.t;
  blocked_relays : (string, unit) Hashtbl.t;
}

let create_state ~publish =
  { publish;
    last_events = [];
    mutex = Eio.Mutex.create ();
    blocked_relays = Hashtbl.create 4;
  }

let normalize_url url =
  let trimmed = String.trim url in
  try
    let uri = Uri.of_string trimmed in
    let scheme = Option.map String.lowercase_ascii (Uri.scheme uri) in
    let host = Option.map String.lowercase_ascii (Uri.host uri) in
    let path =
      match Uri.path uri with
      | "" | "/" -> "/"
      | other -> other
    in
    let normalized =
      Uri.make
        ?scheme
        ?host
        ?port:(Uri.port uri)
        ~path
        ~query:(Uri.query uri)
        ()
    in
    Uri.to_string normalized
  with
  | _ -> trimmed

let block_relay t relay reason =
  Eio.Mutex.lock t.mutex;
  let already_blocked = Hashtbl.mem t.blocked_relays relay in
  if not already_blocked then Hashtbl.add t.blocked_relays relay ();
  Eio.Mutex.unlock t.mutex;
  if not already_blocked then
    traceln "Relay %s blocked for contact publishing (%s)" relay reason

let unblock_relay t relay =
  Eio.Mutex.lock t.mutex;
  let was_blocked = Hashtbl.mem t.blocked_relays relay in
  if was_blocked then Hashtbl.remove t.blocked_relays relay;
  Eio.Mutex.unlock t.mutex;
  if was_blocked then traceln "Relay %s unblocked for contact publishing" relay

let relay_blocked t relay_url =
  let relay = normalize_url relay_url in
  Eio.Mutex.lock t.mutex;
  let blocked = Hashtbl.mem t.blocked_relays relay in
  Eio.Mutex.unlock t.mutex;
  blocked

let filter_allowed_relays t relays =
  List.filter
    (fun relay_url ->
      let blocked = relay_blocked t relay_url in
      if blocked then
        traceln "Skipping publish to %s (blocked)" (normalize_url relay_url);
      not blocked)
    relays

let update_blocklist t results =
  List.iter
    (fun { Nostr_publish.relay; outcome } ->
      match outcome with
      | Nostr_publish.Ack _ -> unblock_relay t relay
      | Nostr_publish.Rejected reason
      | Nostr_publish.Failed reason -> block_relay t relay reason
      | Nostr_publish.Timeout -> ())
    results

let store_events t events =
  Eio.Mutex.lock t.mutex;
  t.last_events <- events;
  Eio.Mutex.unlock t.mutex

let replay_events_to_relay t relay_url =
  if relay_blocked t relay_url then
    traceln "Skipping replay of stored events to %s (blocked)" (normalize_url relay_url)
  else (
    Eio.Mutex.lock t.mutex;
    let events = t.last_events in
    Eio.Mutex.unlock t.mutex;
    List.iter
      (fun event ->
        let results =
          t.publish ~relays:[relay_url] event
        in
        List.iter
          (fun { Nostr_publish.relay; outcome } ->
            match outcome with
            | Nostr_publish.Ack reason ->
              traceln "Replayed event %s to %s (ack: %s)" event.id relay reason
            | Nostr_publish.Rejected reason ->
              traceln "Relay %s rejected replayed event %s: %s" relay event.id reason
            | Nostr_publish.Failed reason ->
              traceln "Failed to replay event %s on %s: %s" event.id relay reason
            | Nostr_publish.Timeout ->
              traceln "Timed out replaying event %s on %s" event.id relay)
          results;
        update_blocklist t results)
      events)

let npub_to_hex npub =
  match Nip19.decode_npub npub with
  | Ok bytes -> Ok (Nostr_event.Hex.bytes_to_hex bytes)
  | Error msg -> Error msg

let build_tags npubs =
  let rec gather acc skipped = function
    | [] -> (List.rev acc, List.rev skipped)
    | npub :: rest ->
      (match npub_to_hex npub with
       | Ok hex -> gather (([ "p"; hex ]) :: acc) skipped rest
       | Error msg -> gather acc ((npub, msg) :: skipped) rest)
  in
  let contact_tags, skipped = gather [] [] npubs in
  (* NIP-78 では "d" タグでスコープを示し "k" で元の種類を明示する *)
  let base_tags = [ [ "d"; "nostr-tossup:recent-contacts" ]; [ "k"; "3" ] ] in
  (base_tags @ contact_tags, skipped)

let log_publish_results label results =
  List.iter
    (fun { Nostr_publish.relay; outcome } ->
      match outcome with
      | Nostr_publish.Ack reason ->
        traceln "Relay %s acknowledged %s (%s)" relay label reason
      | Nostr_publish.Rejected reason ->
        traceln "Relay %s rejected %s: %s" relay label reason
      | Nostr_publish.Failed reason ->
        traceln "Failed to publish %s to %s: %s" label relay reason
      | Nostr_publish.Timeout ->
        traceln "Timed out publishing %s to %s" label relay)
    results

let publish_once t ~sw ~stdenv ~keypair ?uri () =
  Database.with_connection ?uri ~sw ~stdenv @@ fun conn ->
  match User_repository.list_recent_new_users conn with
  | Error err ->
    traceln "Failed to load recent users: %a" Caqti_error.pp err
  | Ok npubs ->
    let tags, skipped = build_tags npubs in
    List.iter (fun (npub, msg) -> traceln "Skipping npub %s: %s" npub msg) skipped;
    let contact_event =
      Nostr_event.signed_event_message
        ~kind:30078
        ~content:""
        ~tags
        ~keypair
    in
    let relays = filter_allowed_relays t Config.publish_relays in
    let contact_results =
      if relays = [] then (
        traceln "No eligible relays for kind 30078 publish; skipping";
        []
      ) else
        t.publish ~relays contact_event
    in
    traceln "Posted kind 30078 contacts (%d users, skipped %d) as %s"
      (List.length npubs)
      (List.length skipped)
      contact_event.id;
    update_blocklist t contact_results;
    log_publish_results "kind 30078 contacts" contact_results;
    let summary_content =
      Printf.sprintf
        "Posted kind 30078 contacts: users=%d skipped=%d"
        (List.length npubs)
        (List.length skipped)
    in
    let summary_event =
      Nostr_event.kind1_event_message
        ~content:summary_content
        ~tags:[]
        ~keypair
    in
    let summary_results =
      if relays = [] then [] else t.publish ~relays summary_event
    in
    traceln "Also posted kind 1 summary as %s" summary_event.id;
    update_blocklist t summary_results;
    log_publish_results "kind 1 summary" summary_results;
    store_events t [ summary_event; contact_event ]

let start ~publisher ~sw ~clock ~stdenv ~keypair ?uri ?(interval = 3600.)
    ?(initial_delay = 0.) () =
  let publish ~relays event = Nostr_publish.publish publisher ~clock ~relays event in
  let state = create_state ~publish in
  let rec loop () =
    (try publish_once state ~sw ~stdenv ~keypair ?uri () with
     | Caqti_error.Exn err ->
       traceln "kind30078 publisher DB error: %a" Caqti_error.pp err
     | exn ->
       traceln "kind30078 publisher error: %s" (Printexc.to_string exn));
    Eio.Time.sleep clock interval;
    loop ()
  in
  Fiber.fork ~sw (fun () ->
      if initial_delay > 0. then Eio.Time.sleep clock initial_delay;
      loop ());
  state

let on_relay_connected t ~sw relay_url =
  Fiber.fork ~sw (fun () -> replay_events_to_relay t relay_url)
