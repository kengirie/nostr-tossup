open Eio.Std
module Ws = Piaf.Ws

(* State record to manage connection state *)
type connection_state = {
  mutable last_message: float;
}

(* Create a dummy keepalive REQ message *)
let create_keepalive_request keepalive_id =
  `List
    [
      `String "REQ";
      `String keepalive_id;
      `Assoc [
        ("ids", `List [`String (String.make 64 'a')]);  (* Invalid ID that doesn't exist *)
      ];
    ]

(* Receive and process WebSocket messages from the Nostr relay *)
let rec recv_loop relay_name messages ws_descriptor state publisher subscriber =
  match Piaf.Stream.take messages with
  | Some (_opcode, iovec) ->
    (* Extract string data from WebSocket frame *)
    let received_string = Bigstringaf.substring iovec.Faraday.buffer ~off:iovec.Faraday.off ~len:iovec.Faraday.len in
    state.last_message <- Unix.gettimeofday ();
    (try
       let json = Yojson.Safe.from_string received_string in
       match json with
       | `List [ `String "EVENT"; `String sub_id; event_obj ] ->
         (match subscriber with
          | Some pool ->
            Nostr_subscribe.handle_event pool ~relay_url:relay_name ~subscription_id:sub_id ~event:event_obj
          | None -> ());
         traceln "Received event from %s: %s" relay_name (Yojson.Safe.to_string event_obj)
       | `List [ `String "EOSE"; `String sub_id ] ->
         (match subscriber with
          | Some pool -> Nostr_subscribe.handle_eose pool ~relay_url:relay_name ~subscription_id:sub_id
          | None -> ())
       | `List [ `String "OK"; `String event_id; status; message_json ] ->
         let ok =
           match status with
           | `Bool b -> b
           | `Int 1 -> true
           | `Int 0 -> false
           | `Intlit s -> (try int_of_string s <> 0 with _ -> false)
           | `String s ->
             (match String.lowercase_ascii s with
              | "true" | "ok" | "yes" -> true
              | _ -> false)
           | _ -> false
         in
         let message =
           match message_json with
           | `String s -> s
           | _ -> Yojson.Safe.to_string message_json
         in
         (match publisher with
          | Some pool -> Nostr_publish.handle_ok pool ~relay_url:relay_name ~event_id ~ok ~message
          | None -> ());
         traceln "[%s] OK for %s (ok=%b message=%s)" relay_name event_id ok message
       | `List [ `String "NOTICE"; message_json ] ->
         let message =
           match message_json with
           | `String s -> s
           | _ -> Yojson.Safe.to_string message_json
         in
         (match publisher with
         | Some pool -> Nostr_publish.handle_notice pool ~relay_url:relay_name ~message
          | None -> ());
         traceln "[%s] NOTICE: %s" relay_name message
       | `List [ `String "CLOSED"; `String sub_id; reason_json ] ->
         let reason =
           match reason_json with
           | `String s -> s
           | _ -> Yojson.Safe.to_string reason_json
         in
         (match subscriber with
          | Some pool -> Nostr_subscribe.handle_closed pool ~relay_url:relay_name ~subscription_id:sub_id ~reason
          | None -> ());
         traceln "[%s] Subscription %s closed: %s" relay_name sub_id reason
       | `List [ `String "AUTH"; challenge_json ] ->
         let payload = Yojson.Safe.to_string challenge_json in
         traceln "[%s] AUTH challenge received: %s" relay_name payload
       | _ -> ()
     with
     | Yojson.Json_error msg ->
       traceln "[%s] JSON parse error: %s" relay_name msg);
    recv_loop relay_name messages ws_descriptor state publisher subscriber
  | None ->
    traceln "[%s] Connection closed" relay_name;
    raise (Failure "Connection closed")

let send_initial_event relay_url ws_descriptor event =
  let open Nostr_event in
  match event with
  | None -> ()
  | Some { message; id; _ } ->
    (try
       Ws.Descriptor.send_string ws_descriptor message;
       traceln "[%s] Published kind 1 event %s" relay_url id
     with e ->
       traceln "[%s] Failed to publish event: %s" relay_url (Printexc.to_string e))

(* Establish a single WebSocket connection to a Nostr relay *)
let connect_to_relay
    ~config
    ~sw
    ?publisher
    ?subscriber
    ?on_relay_connected
    env
    relay_url
    event =
  let uri = Uri.of_string relay_url in
  (* Create connection state *)
  let state = { last_message = Unix.gettimeofday () } in
  match Piaf.Client.create ~config ~sw env uri with
  | Error e ->
    traceln "[%s] Error creating client: %a" relay_url Piaf.Error.pp_hum e;
    raise (Failure "Client creation failed")
  | Ok client ->
    match Piaf.Client.ws_upgrade client "/" with
    | Error e ->
      traceln "[%s] Error upgrading to websocket: %a" relay_url Piaf.Error.pp_hum e;
      raise (Failure "WebSocket upgrade failed")
    | Ok ws_descriptor ->
      traceln "Connected to %s" relay_url;
      Fun.protect
        ~finally:(fun () ->
          match publisher with
          | Some pool -> Nostr_publish.detach_connection pool ~relay_url ~reason:"connection closed"
          | None -> ();
          match subscriber with
          | Some pool -> Nostr_subscribe.detach_connection pool ~relay_url ~reason:"connection closed"
          | None -> ())
        (fun () ->
          try
            send_initial_event relay_url ws_descriptor event;

            (match publisher with
             | Some pool ->
               let send message =
                 try Ws.Descriptor.send_string ws_descriptor message with
                 | exn ->
                   traceln "[%s] Failed to post event: %s" relay_url (Printexc.to_string exn);
                   raise exn
               in
               Nostr_publish.register_connection pool ~relay_url ~send
             | None -> ());

            (match subscriber with
             | Some pool ->
               let send message =
                 try Ws.Descriptor.send_string ws_descriptor message with
                 | exn ->
                   traceln "[%s] Failed to send subscription message: %s" relay_url (Printexc.to_string exn);
                   raise exn
               in
               Nostr_subscribe.register_connection pool ~relay_url ~send;
               traceln "[%s] Subscriber registered" relay_url
             | None -> ());

            (match on_relay_connected with
             | Some callback -> callback ~sw relay_url
             | None -> ());

            let messages = Ws.Descriptor.messages ws_descriptor in

            (* Start Nostr-level keep-alive in parallel with message receiving *)
            Fiber.both
              (fun () ->
                let keepalive_counter = ref 0 in
                let rec keepalive_loop () =
                  Eio.Time.sleep (Eio.Stdenv.clock env) Config.keepalive_interval;

                  (* Check if we received an EOSE recently *)
                  let now = Unix.gettimeofday () in
                  let time_since_message = now -. state.last_message in
                  if time_since_message > (Config.keepalive_interval +. Config.eose_timeout) then (
                    traceln "[%s] Keep-alive timeout (%.1fs since last message)" relay_url time_since_message;
                    raise (Failure "Keep-alive timeout")
                  );

                  (* Send dummy keepalive request *)
                  incr keepalive_counter;
                  let keepalive_id = Printf.sprintf "keepalive_%d" !keepalive_counter in
                  let keepalive_request = create_keepalive_request keepalive_id in
                  let request_string = Yojson.Safe.to_string keepalive_request in
                  (try
                    Ws.Descriptor.send_string ws_descriptor request_string;
                    state.last_message <- Unix.gettimeofday ()
                  with e ->
                    traceln "[%s] Failed to send keepalive: %s" relay_url (Printexc.to_string e);
                    raise e);
                  keepalive_loop ()
                in
                keepalive_loop ())
              (fun () ->
                recv_loop relay_url messages ws_descriptor state publisher subscriber)
          with
          | Failure msg ->
            traceln "[%s] connect_to_relay: Connection error: %s" relay_url msg;
            raise (Failure msg)
          | e ->
            traceln "[%s] connect_to_relay: Unexpected error: %s" relay_url (Printexc.to_string e);
            raise e)

(* Connect to multiple relays concurrently using Eio fibers *)
let connect_to_all_relays ?publisher ?subscriber ?on_relay_connected env config event =
  let connect_to_single_relay relay_url =
    fun () ->
      let rec retry_loop attempt =
        try
          Switch.run (fun sw ->
            connect_to_relay
              ~config
              ~sw
              ?publisher
              ?subscriber
              ?on_relay_connected
              env
              relay_url
              event)
        with
        | Failure msg ->
          traceln "[%s] Connection failed: %s" relay_url msg;
          Eio.Time.sleep (Eio.Stdenv.clock env) Config.reconnect_delay;
          retry_loop (attempt + 1)
        | e ->
          traceln "[%s] Unexpected error: %s" relay_url (Printexc.to_string e);
          Eio.Time.sleep (Eio.Stdenv.clock env) Config.reconnect_delay;
          retry_loop (attempt + 1)
      in
      retry_loop 1
  in
  (* Create fibers for all relays *)
  let relay_fibers = List.map connect_to_single_relay Config.all_relays in
  (* Run all connections concurrently *)
  match relay_fibers with
  | [] -> ()
  | fibers ->
    Fiber.all fibers
