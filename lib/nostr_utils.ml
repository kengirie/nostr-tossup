open Eio.Std

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

module EventBuilder = struct
  let create_signed_event ~kind ~content ~tags ~keypair =
    Nostr_event.signed_event_message ~kind ~content ~tags ~keypair

  let create_kind1_event ~content ~tags ~keypair =
    Nostr_event.kind1_event_message ~content ~tags ~keypair

  let create_kind30078_event ~content ~tags ~keypair =
    create_signed_event ~kind:30078 ~content ~tags ~keypair
end

module RelayBlocklist = struct
  type t = {
    blocked: (string, unit) Hashtbl.t;
    mutex: Eio.Mutex.t;
  }

  let create () = {
    blocked = Hashtbl.create 4;
    mutex = Eio.Mutex.create ();
  }

  let block t relay reason =
    Eio.Mutex.lock t.mutex;
    let already_blocked = Hashtbl.mem t.blocked relay in
    if not already_blocked then Hashtbl.add t.blocked relay ();
    Eio.Mutex.unlock t.mutex;
    if not already_blocked then
      traceln "Relay %s blocked (%s)" relay reason

  let unblock t relay =
    Eio.Mutex.lock t.mutex;
    let was_blocked = Hashtbl.mem t.blocked relay in
    if was_blocked then Hashtbl.remove t.blocked relay;
    Eio.Mutex.unlock t.mutex;
    if was_blocked then 
      traceln "Relay %s unblocked" relay

  let is_blocked t relay_url =
    let relay = normalize_url relay_url in
    Eio.Mutex.lock t.mutex;
    let blocked = Hashtbl.mem t.blocked relay in
    Eio.Mutex.unlock t.mutex;
    blocked

  let filter_allowed_relays t relays =
    List.filter (fun relay_url ->
      let blocked = is_blocked t relay_url in
      if blocked then
        traceln "Skipping relay %s (blocked)" (normalize_url relay_url);
      not blocked
    ) relays
end

module PublishResult = struct
  type outcome =
    | Ack of string
    | Rejected of string  
    | Failed of string
    | Timeout

  type result = {
    relay : string;
    outcome : outcome;
  }

  let log_results label results =
    List.iter (fun {relay; outcome} ->
      match outcome with
      | Ack reason ->
        traceln "Relay %s acknowledged %s (%s)" relay label reason
      | Rejected reason ->
        traceln "Relay %s rejected %s: %s" relay label reason
      | Failed reason ->
        traceln "Failed to publish %s to %s: %s" label relay reason
      | Timeout ->
        traceln "Timed out publishing %s to %s" label relay
    ) results

  let update_blocklist blocklist results =
    List.iter (fun {relay; outcome} ->
      match outcome with
      | Ack _ -> RelayBlocklist.unblock blocklist relay
      | Rejected reason 
      | Failed reason -> RelayBlocklist.block blocklist relay reason
      | Timeout -> ()
    ) results
end

let npub_to_hex npub =
  match Nip19.decode_npub npub with
  | Ok bytes -> Ok (Nostr_event.Hex.bytes_to_hex bytes)
  | Error msg -> Error msg