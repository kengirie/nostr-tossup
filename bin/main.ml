open Eio.Std
module Ws = Piaf.Ws

let nostr_relay = "https://yabu.me"

let main env =
  (* NostrリレーにWebSocketで接続するため、まずHTTP/1.1で接続し、
     その後WebSocketプロトコルにアップグレードします。
     PiafではHTTP/2接続だとアップグレードに失敗するため、
     設定でHTTP/1.1を強制しています。 *)
  let config =
    let open Piaf.Config in
    { default with max_http_version = Piaf.Versions.HTTP.HTTP_1_1 }
  in
  Switch.run (fun sw ->
      let uri = Uri.of_string nostr_relay in
      match Piaf.Client.create ~config ~sw env uri with
      | Error e ->
        traceln "Error creating client: %a" Piaf.Error.pp_hum e
      | Ok client ->
        (* 上記で作成したHTTP/1.1接続を、ここでWebSocketにアップグレードします。 *)
        match Piaf.Client.ws_upgrade client "/" with
        | Error e ->
          traceln "Error upgrading to websocket: %a" Piaf.Error.pp_hum e
        | Ok ws_descriptor ->
          traceln "Connected to %a" Uri.pp_hum uri;
          let subscription_id = "my_sub" in
          let request =
            `List
              [
                `String "REQ";
                `String subscription_id;
                `Assoc [
                  ("kinds", `List [`Int 1]);
                  ("limit", `Int 5)
                ];
              ]
          in
          let request_string = Yojson.Safe.to_string request in
          Ws.Descriptor.send_string ws_descriptor request_string;
          (* JSONを整形して表示 *)
          let pretty_json = Yojson.Safe.pretty_to_string request in
          traceln "Sent JSON:\n%s" pretty_json;

          let messages = Ws.Descriptor.messages ws_descriptor in
          let rec recv_loop () =
            match Piaf.Stream.take messages with
            | Some (_opcode, iovec) ->
              let received_string = Bigstringaf.substring iovec.buffer ~off:iovec.off ~len:iovec.len in
              (try
                 let json = Yojson.Safe.from_string received_string in
                 match json with
                 | `List [ `String "EVENT"; _sub_id; event_obj ] ->
                   traceln "Received event: %s" (Yojson.Safe.to_string event_obj)
                 | _ -> ()
               with
               | Yojson.Json_error msg ->
                 traceln "JSON parse error: %s" msg);
              recv_loop ()
            | None ->
              traceln "Connection closed"
          in
          recv_loop ()
    )

let () = Eio_main.run main
