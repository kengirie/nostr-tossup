open Eio.Std

(* Main application entry point *)
let main env =
  let config =
    let open Piaf.Config in
    { default with max_http_version = Piaf.Versions.HTTP.HTTP_1_1 }
  in
  let subscription_id = Config.subscription_id in

  try
    Nostr_connection.connect_to_all_relays env config subscription_id
  with
  | Failure msg ->
    traceln "Failed to establish connections: %s" msg
  | e ->
    traceln "Unexpected error: %s" (Printexc.to_string e)

let () = Eio_main.run main
