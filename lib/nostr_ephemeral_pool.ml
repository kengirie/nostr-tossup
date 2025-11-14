open Eio.Std
module Ws = Piaf.Ws

(*
   Inspired by the short-lived subscriptions in nbd-wtf/nostr-tools (see abstract-pool.ts)
   we provide a reusable helper that opens a dedicated WebSocket connection for a batch of
   REQ filters, waits until EOSE/closed, then tears everything down. This keeps the
   long-lived relay connections focused on streaming traffic while fire-and-forget
   queries (e.g. periodic kind1 checks) borrow a clean connection each time.
*)

type t = {
  config : Piaf.Config.t;
}

let create ~config = { config }

let counter_mutex = Eio.Mutex.create ()
let sub_counter = ref 0

let next_sub_id ?label () =
  Eio.Mutex.lock counter_mutex;
  let id = !sub_counter in
  incr sub_counter;
  Eio.Mutex.unlock counter_mutex;
  match label with
  | None -> Printf.sprintf "oneshot#%d" id
  | Some lbl -> Printf.sprintf "oneshot#%s#%d" lbl id

let recv_string iovec =
  Bigstringaf.substring iovec.Faraday.buffer ~off:iovec.Faraday.off ~len:iovec.Faraday.len

let yojson_to_string = function
  | `String s -> s
  | json -> Yojson.Safe.to_string json

let contains_substring haystack needle =
  let len_h = String.length haystack in
  let len_n = String.length needle in
  let rec loop i =
    if i + len_n > len_h then false
    else if String.sub haystack i len_n = needle then true
    else loop (i + 1)
  in
  loop 0

let is_closed_writer_error exn =
  match exn with
  | Failure msg when String.equal msg "cannot write to closed writer" -> true
  | exn -> contains_substring (Printexc.to_string exn) "cannot write to closed writer"

let collect_from_relay t ~env ~sw ~clock ~relay_url ~filters ~timeout ?label () =
  let relay = Nostr_utils.normalize_url relay_url in
  let uri = Uri.of_string relay in
  match Piaf.Client.create ~config:t.config ~sw env uri with
  | Error err ->
    traceln "[%s] Ephemeral connect failed: %a" relay Piaf.Error.pp_hum err;
    []
  | Ok client ->
    (match Piaf.Client.ws_upgrade client "/" with
     | Error err ->
       traceln "[%s] Ephemeral websocket upgrade failed: %a" relay Piaf.Error.pp_hum err;
       []
     | Ok ws_descriptor ->
       let sub_id = next_sub_id ?label () in
       let cleanup () =
         traceln "[%s] Ephemeral websocket closing (sub=%s)" relay sub_id;
         (try Ws.Descriptor.close ws_descriptor with exn ->
            if is_closed_writer_error exn then
              traceln "[%s] Ephemeral websocket close skipped (sub=%s)" relay sub_id
            else raise exn);
         (try Piaf.Client.shutdown client with exn ->
            if is_closed_writer_error exn then
              traceln "[%s] Ephemeral client shutdown skipped (sub=%s)" relay sub_id
            else raise exn)
       in
       let known_ids = Hashtbl.create 32 in
       let events = ref [] in
       let finished = ref false in
       let promise, resolver = Promise.create () in
       let resolve () =
         if not !finished then (
           finished := true;
           Promise.resolve resolver ())
       in
       let messages = Ws.Descriptor.messages ws_descriptor in
       let rec reader_loop () =
         if !finished then ()
         else
           try
             match Piaf.Stream.take messages with
             | Some (_opcode, iovec) ->
               let payload = recv_string iovec in
               (try
                  match Yojson.Safe.from_string payload with
                  | `List [ `String "EVENT"; `String sid; event_json ] when sid = sub_id ->
                    (match Nostr_subscribe.event_id event_json with
                     | Some id when not (Hashtbl.mem known_ids id) ->
                       Hashtbl.add known_ids id ();
                       events := event_json :: !events
                     | _ -> ());
                    reader_loop ()
                  | `List [ `String "EOSE"; `String sid ] when sid = sub_id ->
                    traceln "[%s] Ephemeral sub %s received EOSE" relay sub_id;
                    resolve ()
                  | `List [ `String "CLOSED"; `String sid; reason_json ] when sid = sub_id ->
                    traceln "[%s] Ephemeral sub %s closed: %s" relay sub_id (yojson_to_string reason_json);
                    resolve ()
                  | _ -> reader_loop ()
                with
                | Yojson.Json_error msg ->
                  traceln "[%s] Ephemeral parse error: %s" relay msg;
                  reader_loop ()
                | exn ->
                  traceln "[%s] Ephemeral reader error: %s" relay (Printexc.to_string exn);
                  if is_closed_writer_error exn then resolve () else reader_loop ());
             | None -> resolve ()
             with exn ->
             traceln "[%s] Ephemeral stream error (sub=%s): %s" relay sub_id (Printexc.to_string exn);
             if is_closed_writer_error exn then resolve () else reader_loop ()
       in
       Fiber.fork ~sw (fun () -> reader_loop ());
       let req = Nostr_subscribe.build_req sub_id filters in
       Nostr_subscribe.log_req relay sub_id filters;
       (try
          Ws.Descriptor.send_string ws_descriptor req with
        | exn ->
          traceln "[%s] Ephemeral send failed: %s" relay (Printexc.to_string exn);
          if is_closed_writer_error exn then resolve () else raise exn);
       let run () =
         (match Eio.Time.with_timeout clock timeout (fun () -> Ok (Promise.await promise)) with
          | Ok () -> ()
          | Error `Timeout ->
            traceln "[%s] Ephemeral sub %s timed out (%.1fs)" relay sub_id timeout;
            resolve ());
         let close_msg = Nostr_subscribe.build_close sub_id in
         (try
            Ws.Descriptor.send_string ws_descriptor close_msg;
            traceln "[%s] Ephemeral sub %s sent CLOSE" relay sub_id
          with exn ->
            if is_closed_writer_error exn then
              traceln "[%s] Ephemeral sub %s close skipped (writer closed)" relay sub_id
            else
              raise exn);
         List.rev !events
       in
       match
         (try Ok (run ()) with exn ->
            if is_closed_writer_error exn then Ok [] else Error exn)
       with
       | Ok result -> cleanup (); result
       | Error exn -> cleanup (); raise exn)

let query t ~env ~clock ~relays ~filters ~timeout ?label () =
  match relays with
  | [] -> []
  | _ ->
    let deduped_relays =
      relays
      |> List.map Nostr_utils.normalize_url
      |> List.sort_uniq String.compare
    in
    let results = ref [] in
    let known = Hashtbl.create 64 in
    let mutex = Eio.Mutex.create () in
    (try
       Switch.run (fun sw ->
           List.iter
             (fun relay ->
               Fiber.fork ~sw (fun () ->
                   let relay_events =
                     try
                       collect_from_relay t ~env ~sw ~clock ~relay_url:relay ~filters ~timeout ?label ()
                     with exn ->
                       traceln "[%s] Ephemeral query error: %s" relay (Printexc.to_string exn);
                       []
                   in
                   if relay_events <> [] then (
                     Eio.Mutex.lock mutex;
                     List.iter
                       (fun event ->
                         match Nostr_subscribe.event_id event with
                         | Some id when not (Hashtbl.mem known id) ->
                           Hashtbl.add known id ();
                           results := event :: !results
                         | _ -> ())
                       relay_events;
                     Eio.Mutex.unlock mutex)))
             deduped_relays)
     with
     | Eio.Cancel.Cancelled _ as exn ->
       traceln "Ephemeral query cancelled: %s" (Printexc.to_string exn)
     | exn ->
       traceln "Ephemeral query aborted: %s" (Printexc.to_string exn));
    List.rev !results
