open Eio.Std

(* Main application entry point *)
let main env =
  let stdenv = (env :> Caqti_eio.stdenv) in
  (try
     Switch.run (fun sw ->
         Nostr_tossup.Database.apply_sql_file
           ~sw
           ~stdenv
           "sql/backup.sql")
   with
   | Caqti_error.Exn err ->
     traceln "Failed to prepare database: %a" Caqti_error.pp err
   | exn ->
     traceln "Failed to prepare database: %s" (Printexc.to_string exn));

  let config =
    let open Piaf.Config in
    { default with max_http_version = Piaf.Versions.HTTP.HTTP_1_1 }
  in
  let subscription_id = Nostr_tossup.Config.subscription_id in

  try
    Nostr_tossup.Nostr_connection.connect_to_all_relays env config subscription_id None
  with
  | Failure msg ->
    traceln "Failed to establish connections: %s" msg
  | e ->
    traceln "Unexpected error: %s" (Printexc.to_string e)

let () = Eio_main.run main
