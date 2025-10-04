open Eio.Std
module Ws = Piaf.Ws

(* Nostr relay configuration *)
let nostr_relays = [
  "https://relay.damus.io";
  "https://nos.lol";
]

(* Reconnection settings *)
let reconnect_delay = 2.0

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

(* Receive and process WebSocket messages from the Nostr relay *)
let rec recv_loop relay_name messages =
  match Piaf.Stream.take messages with
  | Some (_opcode, iovec) ->
    (* Extract string data from WebSocket frame *)
    let received_string = Bigstringaf.substring iovec.Faraday.buffer ~off:iovec.Faraday.off ~len:iovec.Faraday.len in
    (try
       let json = Yojson.Safe.from_string received_string in
       match json with
       | `List [ `String "EVENT"; _sub_id; event_obj ] ->
         traceln "Received event from %s: %s" relay_name (Yojson.Safe.to_string event_obj)
       | _ -> ()
     with
     | Yojson.Json_error msg ->
       traceln "JSON parse error: %s" msg);
    recv_loop relay_name messages
  | None ->
    traceln "Connection closed for %s" relay_name;
    raise (Failure "Connection closed")

(* Establish a single WebSocket connection to a Nostr relay *)
let connect_to_relay ~config ~sw env relay_url subscription_id =
  let uri = Uri.of_string relay_url in
  traceln "Attempting to connect to %s" relay_url;
  match Piaf.Client.create ~config ~sw env uri with
  | Error e ->
    traceln "Error creating client: %a" Piaf.Error.pp_hum e;
    raise (Failure "Client creation failed")
  | Ok client ->
    match Piaf.Client.ws_upgrade client "/" with
    | Error e ->
      traceln "Error upgrading to websocket: %a" Piaf.Error.pp_hum e;
      raise (Failure "WebSocket upgrade failed")
    | Ok ws_descriptor ->
      traceln "Connected to %a" Uri.pp_hum uri;

      let request = create_request subscription_id in
      let request_string = Yojson.Safe.to_string request in

      (try
         Ws.Descriptor.send_string ws_descriptor request_string;
         let pretty_json = Yojson.Safe.pretty_to_string request in
         traceln "Sent JSON:\n%s" pretty_json;

         let messages = Ws.Descriptor.messages ws_descriptor in
         recv_loop relay_url messages
       with
       | Failure msg ->
         traceln "Connection error: %s" msg;
         raise (Failure msg)
       | e ->
         traceln "Unexpected error: %s" (Printexc.to_string e);
         raise e)


(* Connect to multiple relays concurrently using Eio fibers *)
let connect_to_all_relays env config subscription_id =
  let connect_to_single_relay relay_url =
    fun () ->
      let rec retry_loop () =
        try
          Switch.run (fun sw ->
            connect_to_relay ~config ~sw env relay_url subscription_id
          )
        with
        | Failure msg ->
          traceln "Connection failed to %s: %s" relay_url msg;
          traceln "Waiting %.1f seconds before reconnecting to %s..." reconnect_delay relay_url;
          Eio.Time.sleep (Eio.Stdenv.clock env) reconnect_delay;
          retry_loop ()
        | e ->
          traceln "Unexpected error with %s: %s" relay_url (Printexc.to_string e);
          traceln "Waiting %.1f seconds before reconnecting to %s..." reconnect_delay relay_url;
          Eio.Time.sleep (Eio.Stdenv.clock env) reconnect_delay;
          retry_loop ()
      in
      retry_loop ()
  in

  (* Create fibers for all relays *)
  let relay_fibers = List.map connect_to_single_relay nostr_relays in

  (* Run all connections concurrently *)
  match relay_fibers with
  | [] -> traceln "No relays configured"
  | fibers ->
    Fiber.all fibers

(* Main application entry point *)
let main env =
  let config =
    let open Piaf.Config in
    { default with max_http_version = Piaf.Versions.HTTP.HTTP_1_1 }
  in
  let subscription_id = "my_sub" in

  try
    connect_to_all_relays env config subscription_id;
    traceln "All relay connections completed"
  with
  | Failure msg ->
    traceln "Failed to establish connections: %s" msg
  | e ->
    traceln "Unexpected error: %s" (Printexc.to_string e)

let () = Eio_main.run main
