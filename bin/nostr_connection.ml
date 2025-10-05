open Eio.Std
module Ws = Piaf.Ws

(* State record to manage connection state *)
type connection_state = {
  mutable last_eose: float;
}

(* Create a Nostr REQ message to subscribe to events *)
let create_request subscription_id =
  `List
    [
      `String "REQ";
      `String subscription_id;
      `Assoc [
        ("kinds", `List [`Int 1]);
        ("limit", `Int 5)
      ];
    ]

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
let rec recv_loop relay_name messages ws_descriptor state =
  match Piaf.Stream.take messages with
  | Some (_opcode, iovec) ->
    (* Extract string data from WebSocket frame *)
    let received_string = Bigstringaf.substring iovec.Faraday.buffer ~off:iovec.Faraday.off ~len:iovec.Faraday.len in
    (try
       let json = Yojson.Safe.from_string received_string in
       match json with
       | `List [ `String "EVENT"; _sub_id; event_obj ] ->
         traceln "Received event from %s: %s" relay_name (Yojson.Safe.to_string event_obj)
       | `List [ `String "EOSE"; _sub_id ] ->
         state.last_eose <- Unix.gettimeofday ();
       | _ -> ()
     with
     | Yojson.Json_error msg ->
       traceln "[%s] JSON parse error: %s" relay_name msg);
    recv_loop relay_name messages ws_descriptor state
  | None ->
    traceln "[%s] Connection closed" relay_name;
    raise (Failure "Connection closed")

(* Establish a single WebSocket connection to a Nostr relay *)
let connect_to_relay ~config ~sw env relay_url subscription_id =
  let uri = Uri.of_string relay_url in
  (* Create connection state *)
  let state = { last_eose = Unix.gettimeofday () } in
  
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

      let request = create_request subscription_id in
      let request_string = Yojson.Safe.to_string request in

      (try
         Ws.Descriptor.send_string ws_descriptor request_string;
         let pretty_json = Yojson.Safe.pretty_to_string request in
         traceln "Sent JSON:\n%s" pretty_json;

         let messages = Ws.Descriptor.messages ws_descriptor in

         (* Start Nostr-level keep-alive in parallel with message receiving *)
         Fiber.both
           (fun () ->
             let keepalive_counter = ref 0 in
             let rec keepalive_loop () =
               Eio.Time.sleep (Eio.Stdenv.clock env) Config.keepalive_interval;

               (* Check if we received an EOSE recently *)
               let now = Unix.gettimeofday () in
               let time_since_eose = now -. state.last_eose in
               if time_since_eose > (Config.keepalive_interval +. Config.eose_timeout) then (
                 traceln "[%s] Keep-alive timeout (%.1fs since last EOSE)" relay_url time_since_eose;
                 raise (Failure "Keep-alive timeout")
               );

               (* Send dummy keepalive request *)
               incr keepalive_counter;
               let keepalive_id = Printf.sprintf "keepalive_%d" !keepalive_counter in
               let keepalive_request = create_keepalive_request keepalive_id in
               let request_string = Yojson.Safe.to_string keepalive_request in
               (try
                 Ws.Descriptor.send_string ws_descriptor request_string;
               with e ->
                 traceln "[%s] Failed to send keepalive: %s" relay_url (Printexc.to_string e);
                 raise e);
               keepalive_loop ()
             in
             keepalive_loop ())
           (fun () ->
             recv_loop relay_url messages ws_descriptor state)
       with
       | Failure msg ->
         traceln "[%s] connect_to_relay: Connection error: %s" relay_url msg;
         raise (Failure msg)
       | e ->
         traceln "[%s] connect_to_relay: Unexpected error: %s" relay_url (Printexc.to_string e);
         raise e)

(* Connect to multiple relays concurrently using Eio fibers *)
let connect_to_all_relays env config subscription_id =
  let connect_to_single_relay relay_url =
    fun () ->
      let rec retry_loop attempt =
        try
          Switch.run (fun sw ->
            connect_to_relay ~config ~sw env relay_url subscription_id
          )
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
  let relay_fibers = List.map connect_to_single_relay Config.nostr_relays in
  (* Run all connections concurrently *)
  match relay_fibers with
  | [] -> ()
  | fibers ->
    Fiber.all fibers