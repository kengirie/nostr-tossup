open Eio.Std
module Ws = Piaf.Ws

(* Outcome of a publish attempt to a relay *)
type outcome =
  | Ack of string
  | Rejected of string
  | Failed of string
  | Timeout

(* Result for a relay publish attempt *)
type result = {
  relay : string;
  outcome : outcome;
}

(* Publisher state *)
type t = {
  config : Piaf.Config.t;
  default_timeout : float;
  mutable last_notice : (string * string) option;
  mutex : Eio.Mutex.t;
}

let create ?(default_timeout = 10.0) ~config () =
  { config;
    default_timeout;
    last_notice = None;
    mutex = Eio.Mutex.create ();
  }

let normalize_url = Nostr_utils.normalize_url

let annotate context message = Printf.sprintf "[%s] %s" context message

let piaf_error_to_string err = Format.asprintf "%a" Piaf.Error.pp_hum err

let yojson_to_string = function
  | `String s -> s
  | json -> Yojson.Safe.to_string json

let set_last_notice t relay message =
  Eio.Mutex.lock t.mutex;
  t.last_notice <- Some (relay, message);
  Eio.Mutex.unlock t.mutex

let parse_ok_status = function
  | `Bool b -> b
  | `Int 1 -> true
  | `Int 0 -> false
  | `Intlit s -> (try int_of_string s <> 0 with _ -> false)
  | `String s ->
    (match String.lowercase_ascii s with
     | "true" | "ok" | "yes" -> true
     | _ -> false)
  | _ -> false

let send_event relay ws message =
  try
    Ws.Descriptor.send_string ws message;
    Ok ()
  with exn ->
    let reason = Printexc.to_string exn in
    Error (annotate (Printf.sprintf "send %s" relay) reason)

let rec wait_for_ack t ~relay ~event_id messages =
  match
    (try Ok (Piaf.Stream.take messages) with
     | exn -> Error (annotate "stream take" (Printexc.to_string exn)))
  with
  | Error reason -> Failed reason
  | Ok None -> Failed "connection closed before acknowledgement"
  | Ok (Some (_opcode, iovec)) ->
    let payload =
      Bigstringaf.substring iovec.Faraday.buffer ~off:iovec.Faraday.off ~len:iovec.Faraday.len
    in
    (try
       match Yojson.Safe.from_string payload with
       | `List [ `String "OK"; `String eid; status; message_json ] when String.equal eid event_id ->
         let ok = parse_ok_status status in
         let message = yojson_to_string message_json in
         if ok then Ack message else Rejected message
       | `List [ `String "NOTICE"; message_json ] ->
         let message = yojson_to_string message_json in
         set_last_notice t relay message;
         traceln "[%s] NOTICE: %s" relay message;
         wait_for_ack t ~relay ~event_id messages
       | `List [ `String "CLOSED"; reason_json ] ->
         let message = yojson_to_string reason_json in
         Failed (annotate "relay closed" message)
       | `List [ `String "CLOSED"; _; reason_json ] ->
         let message = yojson_to_string reason_json in
         Failed (annotate "relay closed" message)
       | _ -> wait_for_ack t ~relay ~event_id messages
     with
     | Yojson.Json_error msg ->
       traceln "[%s] JSON parse error: %s" relay msg;
       wait_for_ack t ~relay ~event_id messages)

let publish_to_relay t ~clock ~timeout ~env (event : Nostr_event.signed_event) relay_url =
  let relay = normalize_url relay_url in
  let uri = Uri.of_string relay in
  let attempt () =
    Switch.run (fun sw ->
        match Piaf.Client.create ~config:t.config ~sw env uri with
        | Error err -> Error (annotate "client create" (piaf_error_to_string err))
        | Ok client ->
          match Piaf.Client.ws_upgrade client "/" with
          | Error err -> Error (annotate "websocket upgrade" (piaf_error_to_string err))
          | Ok ws_descriptor ->
            let cleanup () =
              (try Ws.Descriptor.close ws_descriptor with _ -> ());
              (try Piaf.Client.shutdown client with _ -> ())
            in
            Fun.protect
              ~finally:cleanup
              (fun () ->
                match send_event relay ws_descriptor event.message with
                | Error reason -> Error reason
                | Ok () ->
                  let messages = Ws.Descriptor.messages ws_descriptor in
                  let outcome =
                    match Eio.Time.with_timeout clock timeout (fun () -> Ok (wait_for_ack t ~relay ~event_id:event.id messages)) with
                    | Ok result -> result
                    | Error `Timeout -> Timeout
                  in
                  Ok outcome))
  in
  match attempt () with
  | Ok outcome -> { relay; outcome }
  | Error reason -> { relay; outcome = Failed reason }

let publish t ~clock ~env ?timeout ~relays (event : Nostr_event.signed_event) =
  let timeout = Option.value timeout ~default:t.default_timeout in
  let deduped =
    relays
    |> List.map normalize_url
    |> List.sort_uniq String.compare
  in
  List.map (fun relay_url ->
      let event_copy = {
        Nostr_event.json = event.json;
        message = Bytes.to_string (Bytes.of_string event.message);
        id = event.id;
      } in
      publish_to_relay t ~clock ~timeout ~env event_copy relay_url)
    deduped

let last_notice t =
  Eio.Mutex.lock t.mutex;
  let notice = t.last_notice in
  Eio.Mutex.unlock t.mutex;
  notice
