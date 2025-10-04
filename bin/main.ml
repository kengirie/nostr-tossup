open Eio.Std
module Ws = Piaf.Ws

(* Nostr relay configuration *)
let nostr_relay = "https://relay.damus.io"

(* Reconnection settings *)
let max_reconnect_attempts = 5
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
let rec recv_loop messages =
  match Piaf.Stream.take messages with
  | Some (_opcode, iovec) ->
    (* Extract string data from WebSocket frame *)
    let received_string = Bigstringaf.substring iovec.Faraday.buffer ~off:iovec.Faraday.off ~len:iovec.Faraday.len in
    (try
       let json = Yojson.Safe.from_string received_string in
       match json with
       | `List [ `String "EVENT"; _sub_id; event_obj ] ->
         traceln "Received event: %s" (Yojson.Safe.to_string event_obj)
       | _ -> ()
     with
     | Yojson.Json_error msg ->
       traceln "JSON parse error: %s" msg);
    recv_loop messages
  | None ->
    traceln "Connection closed"

(* Establish a single WebSocket connection to the Nostr relay *)
let connect_once ~config ~sw env uri subscription_id =
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
         recv_loop messages
       with
       | Failure msg ->
         traceln "Connection error: %s" msg;
         raise (Failure msg)
       | e ->
         traceln "Unexpected error: %s" (Printexc.to_string e);
         raise e)

(* Retry connection with exponential backoff *)
let rec connect_with_retry ~config env uri subscription_id attempt =
  if attempt > max_reconnect_attempts then (
    traceln "Max reconnection attempts (%d) reached. Giving up." max_reconnect_attempts;
    failwith "Max retries exceeded"
  ) else (
    traceln "Connection attempt %d/%d" attempt max_reconnect_attempts;
    try
      Switch.run (fun sw ->
        connect_once ~config ~sw env uri subscription_id
      )
    with
    | Failure msg ->
      traceln "Connection failed: %s" msg;
      if attempt < max_reconnect_attempts then (
        traceln "Waiting %.1f seconds before retry..." reconnect_delay;
        Eio.Time.sleep (Eio.Stdenv.clock env) reconnect_delay;
        connect_with_retry ~config env uri subscription_id (attempt + 1)
      ) else (
        traceln "All retry attempts exhausted";
        failwith msg
      )
    | e ->
      traceln "Unexpected error: %s" (Printexc.to_string e);
      if attempt < max_reconnect_attempts then (
        traceln "Waiting %.1f seconds before retry..." reconnect_delay;
        Eio.Time.sleep (Eio.Stdenv.clock env) reconnect_delay;
        connect_with_retry ~config env uri subscription_id (attempt + 1)
      ) else
        raise e
  )

(* Main application entry point *)
let main env =
  let config =
    let open Piaf.Config in
    { default with max_http_version = Piaf.Versions.HTTP.HTTP_1_1 }
  in
  let uri = Uri.of_string nostr_relay in
  let subscription_id = "my_sub" in

  try
    connect_with_retry ~config env uri subscription_id 1;
    traceln "Connection completed successfully"
  with
  | Failure msg ->
    traceln "Failed to establish connection: %s" msg
  | e ->
    traceln "Unexpected error: %s" (Printexc.to_string e)

let () = Eio_main.run main
