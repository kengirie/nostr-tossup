open Eio.Std
module Ws = Piaf.Ws

type connection_state = {
  mutable last_message: float;
}

module Filter = struct
  type t =
    { ids : string list option;
      authors : string list option;
      kinds : int list option;
      tags : (string * string list) list;
      since : int option;
      until : int option;
      search : string option;
      limit : int option
    }

  let create
      ?ids
      ?authors
      ?kinds
      ?tags
      ?since
      ?until
      ?search
      ?limit
      () =
    { ids; authors; kinds; tags = Option.value tags ~default:[]; since; until; search; limit }

  let to_yojson t =
    let fields = ref [] in
    (match t.ids with
     | Some ids -> fields := ("ids", `List (List.map (fun id -> `String id) ids)) :: !fields
     | None -> ());
    (match t.authors with
     | Some authors -> fields := ("authors", `List (List.map (fun a -> `String a) authors)) :: !fields
     | None -> ());
    (match t.kinds with
     | Some kinds -> fields := ("kinds", `List (List.map (fun k -> `Int k) kinds)) :: !fields
     | None -> ());
    List.iter
      (fun (label, values) ->
        let key = if String.length label > 0 && label.[0] = '#' then label else "#" ^ label in
        fields := (key, `List (List.map (fun v -> `String v) values)) :: !fields)
      t.tags;
    (match t.since with
     | Some since -> fields := ("since", `Int since) :: !fields
     | None -> ());
    (match t.until with
     | Some until -> fields := ("until", `Int until) :: !fields
     | None -> ());
    (match t.search with
     | Some search -> fields := ("search", `String search) :: !fields
     | None -> ());
    (match t.limit with
     | Some limit -> fields := ("limit", `Int limit) :: !fields
     | None -> ());
    `Assoc (List.rev !fields)
end

let normalize_url url =
  let trimmed = String.trim url in
  try Uri.of_string trimmed |> Uri.to_string with _ -> trimmed

let json_list items = `List items

let build_req sub_id filters =
  let components = `String "REQ" :: `String sub_id :: filters in
  Yojson.Safe.to_string (`List components)

let log_req relay sub_id filters =
  let filters_json = Yojson.Safe.to_string (`List filters) in
  traceln "[%s] Sending REQ %s with filters %s" relay sub_id filters_json

let build_close sub_id =
  Yojson.Safe.to_string (`List [ `String "CLOSE"; `String sub_id ])

let event_id event_json =
  match Yojson.Safe.Util.member "id" event_json with
  | `String id -> Some id
  | _ -> None

let string_member name json =
  match Yojson.Safe.Util.member name json with
  | `String s -> Some s
  | _ -> None

let collect_relay_results tbl =
  Hashtbl.fold (fun _ reason acc -> reason :: acc) tbl []

module Subscription = struct
  type relay_state = {
    sub_id : string;
    filters : Yojson.Safe.t list;
    mutable eose_received : bool;
    mutable close_reason : string option;
  }

  type t =
    { id : string;
      on_event : Yojson.Safe.t -> unit;
      on_eose : (unit -> unit) option;
      on_close : (string list -> unit) option;
      already_have : (string -> bool) option;
      known_ids : (string, unit) Hashtbl.t;
      relay_states : (string, relay_state) Hashtbl.t;
      mutable eose_fired : bool;
      mutable close_fired : bool;
      mutable closed : bool;
    }
end

open Subscription

type relay_entry = {
  mutable send : (string -> unit) option;
  mutable ws_descriptor : Ws.Descriptor.t option;
  mutable connected : bool;
  connection_state : connection_state;
  subscriptions : (string, (t * relay_state)) Hashtbl.t;
}

type t = {
  relays : (string, relay_entry) Hashtbl.t;
  subscriptions : (string, Subscription.t) Hashtbl.t;
  mutable next_id : int;
  mutex : Eio.Mutex.t;
}

let create () =
  { relays = Hashtbl.create 8;
    subscriptions = Hashtbl.create 16;
    next_id = 0;
    mutex = Eio.Mutex.create ();
  }

let ensure_relay_entry t relay =
  match Hashtbl.find_opt t.relays relay with
  | Some entry -> entry
  | None ->
    let entry = { 
      send = None; 
      ws_descriptor = None;
      connected = false;
      connection_state = { last_message = Unix.gettimeofday () };
      subscriptions = Hashtbl.create 8 
    } in
    Hashtbl.add t.relays relay entry;
    entry

let next_subscription_id t =
  let id = t.next_id in
  t.next_id <- t.next_id + 1;
  Printf.sprintf "sub_%d" id

let next_sub_id subscription relay_index =
  Printf.sprintf "%s#%d" subscription.id relay_index

let register_connection t ~relay_url ~send =
  let relay = normalize_url relay_url in
  let send_actions = ref [] in
  Eio.Mutex.lock t.mutex;
  let entry = ensure_relay_entry t relay in
  entry.send <- Some send;
  entry.connected <- true;
  let mutable_count = ref 0 in
  Hashtbl.iter
    (fun _sub_id (_subscription, relay_state) ->
      relay_state.eose_received <- false;
      relay_state.close_reason <- None;
      incr mutable_count;
      send_actions := (relay, send, relay_state.sub_id, relay_state.filters) :: !send_actions)
    entry.subscriptions;
  Eio.Mutex.unlock t.mutex;
  List.iter
    (fun (relay_name, send_fn, sub_id, filters) ->
      let req = build_req sub_id filters in
      try send_fn req with exn -> traceln "[%s] Failed to send REQ: %s" relay_name (Printexc.to_string exn))
    !send_actions

let detach_connection t ~relay_url ~reason =
  let relay = normalize_url relay_url in
  Eio.Mutex.lock t.mutex;
  (match Hashtbl.find_opt t.relays relay with
   | None -> ()
   | Some entry ->
     entry.send <- None;
     entry.connected <- false;
     entry.ws_descriptor <- None;
     Hashtbl.iter
       (fun _sub_id (subscription, relay_state) ->
         relay_state.eose_received <- false;
         relay_state.close_reason <- Some reason;
         subscription.close_fired <- false)
       entry.subscriptions);
  Eio.Mutex.unlock t.mutex

let broadcast_close subscription =
  match subscription.on_close with
  | None -> ()
  | Some handler ->
    if not subscription.close_fired then (
      subscription.close_fired <- true;
      let reasons =
        Hashtbl.fold
          (fun _relay relay_state acc ->
            match relay_state.close_reason with
            | Some reason -> reason :: acc
            | None -> acc)
          subscription.relay_states
          []
      in
      handler reasons)

let maybe_fire_eose subscription =
  if not subscription.eose_fired then (
    let all_eose =
      Hashtbl.fold (fun _ state acc -> acc && state.eose_received) subscription.relay_states true
    in
    if all_eose then (
      subscription.eose_fired <- true;
      match subscription.on_eose with
      | None -> ()
      | Some handler -> handler ()))

let subscribe
    t
    ~relays
    ~filters
    ?on_eose
    ?on_close
    ?already_have
    ~on_event
    () =
  let send_actions = ref [] in
  let subscription =
    { id = next_subscription_id t;
      on_event;
      on_eose;
      on_close;
      already_have;
      known_ids = Hashtbl.create 32;
      relay_states = Hashtbl.create 8;
      eose_fired = false;
      close_fired = false;
      closed = false;
    }
  in
  Eio.Mutex.lock t.mutex;
  Hashtbl.add t.subscriptions subscription.id subscription;
  traceln "Created subscription %s for %d relays" subscription.id (List.length relays);
  relays
  |> List.map normalize_url
  |> List.sort_uniq String.compare
  |> List.iteri (fun idx relay ->
         let entry = ensure_relay_entry t relay in
         let sub_id = next_sub_id subscription idx in
         let relay_state =
           { sub_id;
             filters;
             eose_received = false;
             close_reason = None;
           }
         in
         Hashtbl.add subscription.relay_states relay relay_state;
         Hashtbl.replace entry.subscriptions sub_id (subscription, relay_state);
         (match entry.send with
          | Some send_fn -> send_actions := (relay, send_fn, sub_id, filters) :: !send_actions
          | None -> ()));
  Eio.Mutex.unlock t.mutex;
  List.iter
    (fun (relay_name, send_fn, sub_id, filters) ->
      let req = build_req sub_id filters in
      log_req relay_name sub_id filters;
      try send_fn req with exn -> traceln "[%s] Failed to send REQ: %s" relay_name (Printexc.to_string exn))
    !send_actions;
  subscription

let close_subscription t subscription ?reason () =
  let close_msg = reason in
  let send_actions = ref [] in
  Eio.Mutex.lock t.mutex;
  if subscription.closed then
    Eio.Mutex.unlock t.mutex
  else begin
    subscription.closed <- true;
    Hashtbl.remove t.subscriptions subscription.id;
    Hashtbl.iter
      (fun relay relay_state ->
        match Hashtbl.find_opt t.relays relay with
        | None -> ()
        | Some entry ->
          Hashtbl.remove entry.subscriptions relay_state.sub_id;
          (match entry.send with
           | Some send_fn -> send_actions := (relay, send_fn, relay_state.sub_id) :: !send_actions
           | None -> ());
          relay_state.close_reason <- close_msg)
      subscription.relay_states;
    Eio.Mutex.unlock t.mutex;
    List.iter
      (fun (relay_name, send_fn, sub_id) ->
        let msg = build_close sub_id in
        try send_fn msg with exn -> traceln "[%s] Failed to send CLOSE: %s" relay_name (Printexc.to_string exn))
      !send_actions;
    broadcast_close subscription
  end

let handle_event t ~relay_url ~subscription_id ~event =
  let callback_opt = ref None in
  Eio.Mutex.lock t.mutex;
  (match Hashtbl.find_opt t.relays (normalize_url relay_url) with
   | None -> ()
   | Some entry ->
     (match Hashtbl.find_opt entry.subscriptions subscription_id with
      | None -> ()
      | Some (subscription, _relay_state) ->
        if not subscription.closed then begin
          let event_id = event_id event in
          let already_seen =
            match event_id with
            | None -> false
            | Some id ->
              (match subscription.already_have with
               | Some fn when fn id -> true
               | _ ->
                 if Hashtbl.mem subscription.known_ids id then true
                 else (Hashtbl.add subscription.known_ids id (); false))
          in
          if not already_seen then
            callback_opt := Some subscription.on_event
        end));
  Eio.Mutex.unlock t.mutex;
  (match !callback_opt with
   | None -> ()
   | Some cb ->
     (try cb event with exn -> traceln "Subscription handler raised: %s" (Printexc.to_string exn)))

let handle_eose t ~relay_url ~subscription_id =
  Eio.Mutex.lock t.mutex;
  (match Hashtbl.find_opt t.relays (normalize_url relay_url) with
   | None -> ()
   | Some entry ->
     (match Hashtbl.find_opt entry.subscriptions subscription_id with
      | None -> ()
      | Some (subscription, relay_state) ->
        relay_state.eose_received <- true;
        maybe_fire_eose subscription));
  Eio.Mutex.unlock t.mutex

let handle_closed t ~relay_url ~subscription_id ~reason =
  let subscription_opt = ref None in
  Eio.Mutex.lock t.mutex;
  (match Hashtbl.find_opt t.relays (normalize_url relay_url) with
   | None -> ()
   | Some entry ->
     (match Hashtbl.find_opt entry.subscriptions subscription_id with
      | None -> ()
      | Some (subscription, relay_state) ->
        relay_state.close_reason <- Some reason;
        if not subscription.close_fired then (
          let all_closed =
            Hashtbl.fold
              (fun _ state acc -> acc && Option.is_some state.close_reason)
              subscription.relay_states
              true
          in
          if all_closed then subscription_opt := Some subscription)));
  Eio.Mutex.unlock t.mutex;
  (match !subscription_opt with
   | None -> ()
   | Some subscription -> broadcast_close subscription)

let create_keepalive_request keepalive_id =
  `List [
    `String "REQ";
    `String keepalive_id;
    `Assoc [
      ("ids", `List [`String (String.make 64 'a')]);
    ];
  ]

let send_initial_event _relay_url ws_descriptor event =
  let open Nostr_event in
  match event with
  | None -> ()
  | Some { message; _ } ->
    (try
       Ws.Descriptor.send_string ws_descriptor message
     with _ -> ())

let rec recv_loop relay_name messages ws_descriptor state t publisher =
  match Piaf.Stream.take messages with
  | Some (_opcode, iovec) ->
    let received_string = Bigstringaf.substring iovec.Faraday.buffer ~off:iovec.Faraday.off ~len:iovec.Faraday.len in
    state.last_message <- Unix.gettimeofday ();
    (try
       let json = Yojson.Safe.from_string received_string in
       match json with
       | `List [ `String "EVENT"; `String sub_id; event_obj ] ->
         handle_event t ~relay_url:relay_name ~subscription_id:sub_id ~event:event_obj
       | `List [ `String "EOSE"; `String sub_id ] ->
         handle_eose t ~relay_url:relay_name ~subscription_id:sub_id
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
       | `List [ `String "NOTICE"; message_json ] ->
         let message =
           match message_json with
           | `String s -> s
           | _ -> Yojson.Safe.to_string message_json
         in
         (match publisher with
         | Some pool -> Nostr_publish.handle_notice pool ~relay_url:relay_name ~message
          | None -> ());
       | `List [ `String "CLOSED"; `String sub_id; reason_json ] ->
         let reason =
           match reason_json with
           | `String s -> s
           | _ -> Yojson.Safe.to_string reason_json
         in
         handle_closed t ~relay_url:relay_name ~subscription_id:sub_id ~reason
       | `List [ `String "AUTH"; challenge_json ] ->
         let payload = Yojson.Safe.to_string challenge_json in
         traceln "[%s] AUTH challenge received: %s" relay_name payload
       | _ -> ()
     with
     | Yojson.Json_error msg ->
       traceln "[%s] JSON parse error: %s" relay_name msg);
    recv_loop relay_name messages ws_descriptor state t publisher
  | None ->
    traceln "[%s] Connection closed" relay_name;
    raise (Failure "Connection closed")

let connect_to_relay t ~config ~sw ?publisher ?on_relay_connected env relay_url event =
  let relay = normalize_url relay_url in
  let entry = ensure_relay_entry t relay in
  let uri = Uri.of_string relay_url in
  
  traceln "[%s] Attempting to connect..." relay_url;
  match Piaf.Client.create ~config ~sw env uri with
  | Error e ->
    traceln "[%s] ❌ Error creating client: %a" relay_url Piaf.Error.pp_hum e;
    Error "Client creation failed"
  | Ok client ->
    match Piaf.Client.ws_upgrade client "/" with
    | Error e ->
      traceln "[%s] ❌ Error upgrading to websocket: %a" relay_url Piaf.Error.pp_hum e;
      Error "WebSocket upgrade failed"
    | Ok ws_descriptor ->
      entry.ws_descriptor <- Some ws_descriptor;
      entry.connected <- true;
      traceln "[%s] ✅ Successfully connected" relay_url;
      
      Fun.protect
        ~finally:(fun () ->
          entry.connected <- false;
          entry.ws_descriptor <- None;
          entry.send <- None;
          match publisher with
          | Some pool -> Nostr_publish.detach_connection pool ~relay_url ~reason:"connection closed"
          | None -> ())
        (fun () ->
          try
            send_initial_event relay_url ws_descriptor event;

            let send message =
              try Ws.Descriptor.send_string ws_descriptor message with
              | exn ->
                traceln "[%s] Failed to send message: %s" relay_url (Printexc.to_string exn);
                raise exn
            in
            entry.send <- Some send;

            (match publisher with
             | Some pool -> Nostr_publish.register_connection pool ~relay_url ~send
             | None -> ());

            (* Register this connection for subscription management *)
            register_connection t ~relay_url ~send;

            (match on_relay_connected with
             | Some callback -> callback ~sw relay_url
             | None -> ());

            let messages = Ws.Descriptor.messages ws_descriptor in

            Fiber.both
              (fun () ->
                let keepalive_counter = ref 0 in
                let rec keepalive_loop () =
                  Eio.Time.sleep (Eio.Stdenv.clock env) Config.keepalive_interval;

                  let now = Unix.gettimeofday () in
                  let time_since_message = now -. entry.connection_state.last_message in
                  if time_since_message > (Config.keepalive_interval +. Config.eose_timeout) then (
                    traceln "[%s] Keep-alive timeout (%.1fs since last message)" relay_url time_since_message;
                    raise (Failure "Keep-alive timeout")
                  );

                  incr keepalive_counter;
                  let keepalive_id = Printf.sprintf "keepalive_%d" !keepalive_counter in
                  let keepalive_request = create_keepalive_request keepalive_id in
                  let request_string = Yojson.Safe.to_string keepalive_request in
                  (try
                    send request_string;
                    entry.connection_state.last_message <- Unix.gettimeofday ()
                  with e ->
                    traceln "[%s] Failed to send keepalive: %s" relay_url (Printexc.to_string e);
                    raise e);
                  keepalive_loop ()
                in
                keepalive_loop ())
              (fun () ->
                recv_loop relay_url messages ws_descriptor entry.connection_state t publisher);
            Ok ()
          with
          | Failure msg ->
            traceln "[%s] ❌ Connection error: %s" relay_url msg;
            Error msg
          | e ->
            traceln "[%s] ❌ Unexpected error: %s" relay_url (Printexc.to_string e);
            Error (Printexc.to_string e))

let connect_to_all_relays t ?publisher ?on_relay_connected env config event =
  let relays_to_connect = Config.subscribe_relays @ Config.publish_relays |> List.sort_uniq String.compare in
  let connect_to_single_relay relay_url =
    fun () ->
      let rec retry_loop attempt =
        try
          Switch.run (fun sw ->
            match connect_to_relay t ~config ~sw ?publisher ?on_relay_connected env relay_url event with
            | Ok () -> ()
            | Error msg -> raise (Failure msg))
        with
        | Failure msg ->
          traceln "[%s] 🔄 Connection failed (attempt %d): %s. Retrying in %.1fs..." 
            relay_url attempt msg Config.reconnect_delay;
          Eio.Time.sleep (Eio.Stdenv.clock env) Config.reconnect_delay;
          retry_loop (attempt + 1)
        | e ->
          traceln "[%s] 🔄 Unexpected error (attempt %d): %s. Retrying in %.1fs..." 
            relay_url attempt (Printexc.to_string e) Config.reconnect_delay;
          Eio.Time.sleep (Eio.Stdenv.clock env) Config.reconnect_delay;
          retry_loop (attempt + 1)
      in
      retry_loop 1
  in
  let relay_fibers = List.map connect_to_single_relay relays_to_connect in
  match relay_fibers with
  | [] -> ()
  | fibers -> Fiber.all fibers

let handle_notice _t ~relay_url ~message =
  traceln "[%s] NOTICE: %s" relay_url message

let subscription_close t subscription =
  close_subscription t subscription ()

let subscription_close_with_reason t subscription reason =
  close_subscription t subscription ~reason ()

let close_all t =
  let subscriptions = ref [] in
  Eio.Mutex.lock t.mutex;
  Hashtbl.iter (fun _ sub -> subscriptions := sub :: !subscriptions) t.subscriptions;
  Eio.Mutex.unlock t.mutex;
  List.iter (fun sub -> subscription_close_with_reason t sub "shutdown") !subscriptions

let close t = close_all t

(* Convenience for clients *)
let subscribe_simple t ~relays ~filter ~on_event ?on_eose ?on_close ?already_have () =
  subscribe t ~relays ~filters:[filter] ?on_eose ?on_close ?already_have ~on_event ()

let close_subscription_handle t subscription = subscription_close t subscription

let close_subscription_handle_with_reason t subscription reason =
  subscription_close_with_reason t subscription reason

(* Expose sub_id for debugging *)
let active_subscriptions t =
  Eio.Mutex.lock t.mutex;
  let subs = Hashtbl.fold (fun key _ acc -> key :: acc) t.subscriptions [] in
  Eio.Mutex.unlock t.mutex;
  subs

let list_connection_status t =
  Eio.Mutex.lock t.mutex;
  let status = Hashtbl.fold (fun url entry acc -> 
    (url, entry.connected) :: acc
  ) t.relays [] in
  Eio.Mutex.unlock t.mutex;
  status

let is_relay_connected t relay_url =
  let relay = normalize_url relay_url in
  Eio.Mutex.lock t.mutex;
  let connected = match Hashtbl.find_opt t.relays relay with
    | Some entry -> entry.connected
    | None -> false
  in
  Eio.Mutex.unlock t.mutex;
  connected

let connect_to_relays = connect_to_all_relays
