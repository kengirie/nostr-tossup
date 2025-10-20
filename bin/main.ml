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
   | Caqti_error.Exn _ -> ()
   | _ -> ());

  let config =
    let open Piaf.Config in
    { default with max_http_version = Piaf.Versions.HTTP.HTTP_1_1 }
  in
  let keypair =
    match Nostr_tossup.Env.load_nsec_secret () with
    | Ok secret -> Nostr_tossup.Bip340.load_secret secret
    | Error _ ->
      failwith "Missing NOSTR_NSEC secret"
  in
  let clock = Eio.Stdenv.clock env in
  let publisher = Nostr_tossup.Nostr_publish.create () in
  let subscriber = Nostr_tossup.Nostr_subscribe.create () in
  let run_connections () =
    Switch.run (fun sw ->
        let contacts_publisher =
          Nostr_tossup.Kind30078_publisher.start
            ~initial_delay:20.
            ~publisher
            ~sw
            ~clock
            ~stdenv
            ~keypair
            ()
        in
        let enqueue_candidate =
          Nostr_tossup.User_ingest.start
            ~sw
            ~clock
            ~stdenv
            ()
        in
        let kind1_filter =
          Nostr_tossup.Nostr_subscribe.Filter.(to_yojson (create ~kinds:[1] ~limit:5 ()))
        in
        (* Set up subscription first *)
        let _kind1_subscription =
          Nostr_tossup.Nostr_subscribe.subscribe_simple
            subscriber
            ~relays:Nostr_tossup.Config.subscribe_relays
            ~filter:kind1_filter
            ~on_event:(fun event ->
              Nostr_tossup.User_ingest.handle_kind1_event enqueue_candidate event)
            ()
        in
        (* Then connect to relays *)
        Nostr_tossup.Nostr_subscribe.connect_to_relays
          subscriber
          ~publisher
          ~on_relay_connected:(Nostr_tossup.Kind30078_publisher.on_relay_connected contacts_publisher)
          env config None)
  in

  (try
     run_connections ()
   with
   | Failure _ -> ()
   | _ -> ())

let () = Eio_main.run main
