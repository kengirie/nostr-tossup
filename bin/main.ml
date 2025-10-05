open Eio.Std
module Ws = Piaf.Ws

(* Nostr relay configuration *)
let nostr_relays = [
  "https://relay.damus.io";
  "https://nos.lol";
]

(* Reconnection settings *)
let reconnect_delay = 2.0

(* Keep-alive settings *)
let keepalive_interval = 30.0  (* Send keepalive REQ every 30 seconds *)
let eose_timeout = 10.0        (* Wait 10 seconds for EOSE response *)

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

(* Keep track of last EOSE received *)
let last_eose = ref (Unix.gettimeofday ())

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
let rec recv_loop relay_name messages ws_descriptor =
  traceln "[%s] recv_loop: Starting message receive attempt" relay_name;
  match Piaf.Stream.take messages with
  | Some (_opcode, iovec) ->
    traceln "[%s] recv_loop: Received WebSocket frame, size=%d" relay_name iovec.Faraday.len;
    
    (* Extract string data from WebSocket frame *)
    let received_string = Bigstringaf.substring iovec.Faraday.buffer ~off:iovec.Faraday.off ~len:iovec.Faraday.len in
    traceln "[%s] recv_loop: Extracted string data, parsing JSON..." relay_name;
    (try
       let json = Yojson.Safe.from_string received_string in
       traceln "[%s] recv_loop: JSON parsed successfully, checking message type" relay_name;
       match json with
       | `List [ `String "EVENT"; _sub_id; event_obj ] ->
         traceln "Received event from %s: %s" relay_name (Yojson.Safe.to_string event_obj)
       | `List [ `String "EOSE"; sub_id ] ->
         traceln "[%s] recv_loop: Received EOSE for subscription: %s" relay_name (Yojson.Safe.to_string sub_id);
         last_eose := Unix.gettimeofday ();
       | _ -> 
         traceln "[%s] recv_loop: Other message received: %s" relay_name received_string
     with
     | Yojson.Json_error msg ->
       traceln "[%s] recv_loop: JSON parse error: %s" relay_name msg);
    traceln "[%s] recv_loop: Message processed, continuing loop..." relay_name;
    recv_loop relay_name messages ws_descriptor
  | None ->
    traceln "[%s] recv_loop: Connection closed detected" relay_name;
    raise (Failure "Connection closed")

(* Establish a single WebSocket connection to a Nostr relay *)
let connect_to_relay ~config ~sw env relay_url subscription_id =
  let uri = Uri.of_string relay_url in
  traceln "[%s] connect_to_relay: Starting connection attempt" relay_url;
  traceln "[%s] connect_to_relay: Creating Piaf client..." relay_url;
  match Piaf.Client.create ~config ~sw env uri with
  | Error e ->
    traceln "[%s] connect_to_relay: Error creating client: %a" relay_url Piaf.Error.pp_hum e;
    raise (Failure "Client creation failed")
  | Ok client ->
    traceln "[%s] connect_to_relay: Client created successfully, upgrading to WebSocket..." relay_url;
    match Piaf.Client.ws_upgrade client "/" with
    | Error e ->
      traceln "[%s] connect_to_relay: Error upgrading to websocket: %a" relay_url Piaf.Error.pp_hum e;
      raise (Failure "WebSocket upgrade failed")
    | Ok ws_descriptor ->
      traceln "[%s] connect_to_relay: WebSocket connection established" relay_url;

      let request = create_request subscription_id in
      let request_string = Yojson.Safe.to_string request in
      traceln "[%s] connect_to_relay: Sending subscription request..." relay_url;

      (try
         Ws.Descriptor.send_string ws_descriptor request_string;
         let pretty_json = Yojson.Safe.pretty_to_string request in
         traceln "Sent JSON:\n%s" pretty_json;
         traceln "[%s] connect_to_relay: Subscription sent, starting message loop..." relay_url;

         let messages = Ws.Descriptor.messages ws_descriptor in
         traceln "[%s] connect_to_relay: Got message stream, starting keep-alive and recv_loop..." relay_url;
         
         (* Start Nostr-level keep-alive in parallel with message receiving *)
         Fiber.both
           (fun () ->
             traceln "[%s] keep_alive: Starting keepalive fiber" relay_url;
             let keepalive_counter = ref 0 in
             let rec keepalive_loop () =
               traceln "[%s] keep_alive: Sleeping for %.1f seconds..." relay_url keepalive_interval;
               Eio.Time.sleep (Eio.Stdenv.clock env) keepalive_interval;
               
               (* Check if we received an EOSE recently *)
               let now = Unix.gettimeofday () in
               let time_since_eose = now -. !last_eose in
               if time_since_eose > (keepalive_interval +. eose_timeout) then (
                 traceln "[%s] keep_alive: No EOSE received for %.1f seconds, connection dead" relay_url time_since_eose;
                 raise (Failure "Keep-alive timeout")
               );
               
               (* Send dummy keepalive request *)
               incr keepalive_counter;
               let keepalive_id = Printf.sprintf "keepalive_%d" !keepalive_counter in
               traceln "[%s] keep_alive: Sending keepalive REQ: %s" relay_url keepalive_id;
               let keepalive_request = create_keepalive_request keepalive_id in
               let request_string = Yojson.Safe.to_string keepalive_request in
               (try
                 Ws.Descriptor.send_string ws_descriptor request_string;
                 traceln "[%s] keep_alive: Keepalive REQ sent successfully" relay_url;
               with e ->
                 traceln "[%s] keep_alive: Failed to send keepalive REQ: %s" relay_url (Printexc.to_string e);
                 raise e);
               keepalive_loop ()
             in
             keepalive_loop ())
           (fun () ->
             traceln "[%s] recv_loop: Starting message receive fiber" relay_url;
             recv_loop relay_url messages ws_descriptor)
       with
       | Failure msg ->
         traceln "[%s] connect_to_relay: Connection error: %s" relay_url msg;
         raise (Failure msg)
       | e ->
         traceln "[%s] connect_to_relay: Unexpected error: %s" relay_url (Printexc.to_string e);
         raise e)


(* Connect to multiple relays concurrently using Eio fibers *)
let connect_to_all_relays env config subscription_id =
  traceln "connect_to_all_relays: Starting multi-relay connection setup";
  let connect_to_single_relay relay_url =
    fun () ->
      traceln "[%s] fiber: Starting fiber for relay" relay_url;
      let rec retry_loop attempt =
        traceln "[%s] fiber: retry_loop attempt #%d" relay_url attempt;
        try
          traceln "[%s] fiber: Creating switch for connection attempt" relay_url;
          Switch.run (fun sw ->
            traceln "[%s] fiber: Switch created, calling connect_to_relay" relay_url;
            connect_to_relay ~config ~sw env relay_url subscription_id
          )
        with
        | Failure msg ->
          traceln "[%s] fiber: Connection failed: %s" relay_url msg;
          traceln "[%s] fiber: Waiting %.1f seconds before retry #%d..." relay_url reconnect_delay (attempt + 1);
          Eio.Time.sleep (Eio.Stdenv.clock env) reconnect_delay;
          retry_loop (attempt + 1)
        | e ->
          traceln "[%s] fiber: Unexpected error: %s" relay_url (Printexc.to_string e);
          traceln "[%s] fiber: Waiting %.1f seconds before retry #%d..." relay_url reconnect_delay (attempt + 1);
          Eio.Time.sleep (Eio.Stdenv.clock env) reconnect_delay;
          retry_loop (attempt + 1)
      in
      retry_loop 1
  in

  traceln "connect_to_all_relays: Creating fibers for %d relays" (List.length nostr_relays);
  (* Create fibers for all relays *)
  let relay_fibers = List.map connect_to_single_relay nostr_relays in

  (* Run all connections concurrently *)
  match relay_fibers with
  | [] -> 
    traceln "connect_to_all_relays: No relays configured";
  | fibers ->
    traceln "connect_to_all_relays: Starting %d fibers concurrently" (List.length fibers);
    Fiber.all fibers;
    traceln "connect_to_all_relays: All fibers completed"

(* Main application entry point *)
let main env =
  traceln "main: Application starting up";
  let config =
    let open Piaf.Config in
    { default with max_http_version = Piaf.Versions.HTTP.HTTP_1_1 }
  in
  let subscription_id = "my_sub" in
  traceln "main: Configuration created, subscription_id=%s" subscription_id;

  try
    traceln "main: Starting relay connections...";
    connect_to_all_relays env config subscription_id;
    traceln "main: All relay connections completed"
  with
  | Failure msg ->
    traceln "main: Failed to establish connections: %s" msg
  | e ->
    traceln "main: Unexpected error: %s" (Printexc.to_string e)

let () = Eio_main.run main
