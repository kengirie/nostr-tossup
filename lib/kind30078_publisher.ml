open Eio.Std

type publish_fn = relays:string list -> Nostr_event.signed_event -> Nostr_publish.result list

type t = {
  publish : publish_fn;
  blocked_relays : Nostr_utils.RelayBlocklist.t;
}

let create_state ~publish =
  { publish; blocked_relays = Nostr_utils.RelayBlocklist.create () }


let block_relay t relay reason =
  Nostr_utils.RelayBlocklist.block t.blocked_relays relay reason

let unblock_relay t relay =
  Nostr_utils.RelayBlocklist.unblock t.blocked_relays relay

let relay_blocked t relay_url =
  Nostr_utils.RelayBlocklist.is_blocked t.blocked_relays relay_url

let filter_allowed_relays t relays =
  Nostr_utils.RelayBlocklist.filter_allowed_relays t.blocked_relays relays

let update_blocklist t results =
  let utils_results = List.map (fun {Nostr_publish.relay; outcome} ->
    let utils_outcome = match outcome with
      | Nostr_publish.Ack reason -> Nostr_utils.PublishResult.Ack reason
      | Nostr_publish.Rejected reason -> Nostr_utils.PublishResult.Rejected reason
      | Nostr_publish.Failed reason -> Nostr_utils.PublishResult.Failed reason
      | Nostr_publish.Timeout -> Nostr_utils.PublishResult.Timeout
    in
    {Nostr_utils.PublishResult.relay; outcome = utils_outcome}
  ) results in
  Nostr_utils.PublishResult.update_blocklist t.blocked_relays utils_results

let npub_to_hex = Nostr_utils.npub_to_hex

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
  let utils_results = List.map (fun {Nostr_publish.relay; outcome} ->
    let utils_outcome = match outcome with
      | Nostr_publish.Ack reason -> Nostr_utils.PublishResult.Ack reason
      | Nostr_publish.Rejected reason -> Nostr_utils.PublishResult.Rejected reason
      | Nostr_publish.Failed reason -> Nostr_utils.PublishResult.Failed reason
      | Nostr_publish.Timeout -> Nostr_utils.PublishResult.Timeout
    in
    {Nostr_utils.PublishResult.relay; outcome = utils_outcome}
  ) results in
  Nostr_utils.PublishResult.log_results label utils_results

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
    log_publish_results "kind 1 summary" summary_results

let start ~publisher ~sw ~clock ~stdenv ~keypair ~env ?uri ?(interval = 300.)
    ?(initial_delay = 0.) () =
  let publish ~relays event = Nostr_publish.publish publisher ~clock ~env ~relays event in
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
  ()
