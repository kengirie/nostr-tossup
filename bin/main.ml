open Eio.Std

(* Main application entry point *)
let main env =
  let config =
    let open Piaf.Config in
    { default with max_http_version = Piaf.Versions.HTTP.HTTP_1_1 }
  in
  let subscription_id = Nostr_tossup.Config.subscription_id in

  let secret =
    match Nostr_tossup.Env.load_nsec_secret () with
    | Ok secret -> secret
    | Error msg ->
      traceln "NOSTR_NSEC not available (%s) – using dummy secret" msg;
      Bytes.make 32 '\001'
  in
  let keypair = Nostr_tossup.Bip340.load_secret secret in
  let content = "Test" in
  let event =
    Nostr_tossup.Nostr_event.kind1_event_message
      ~content
      ~tags:[]
      ~keypair
  in
  traceln "Prepared kind 1 event %s" event.id;

  try
    Nostr_tossup.Nostr_connection.connect_to_all_relays env config subscription_id (Some event)
  with
  | Failure msg ->
    traceln "Failed to establish connections: %s" msg
  | e ->
    traceln "Unexpected error: %s" (Printexc.to_string e)

let () = Eio_main.run main
