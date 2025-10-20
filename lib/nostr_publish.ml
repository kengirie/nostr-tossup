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

(* Internal state for a waiter awaiting an acknowledgement *)
type waiter = {
  cond : Eio.Condition.t;
  mutable outcome : outcome option;
}

(* State tracked per relay connection *)
type relay_entry = {
  mutable send : (string -> unit) option;
  waiting : (string, waiter list) Hashtbl.t;
}

(* Publisher state *)
type t = {
  relays : (string, relay_entry) Hashtbl.t;
  mutex : Eio.Mutex.t;
  default_timeout : float;
  mutable last_notice : (string * string) option;
}

let create ?(default_timeout = 5.0) () =
  { relays = Hashtbl.create 8;
    mutex = Eio.Mutex.create ();
    default_timeout;
    last_notice = None;
  }

let normalize_url = Nostr_utils.normalize_url

let ensure_relay_entry t relay_url =
  match Hashtbl.find_opt t.relays relay_url with
  | Some entry -> entry
  | None ->
    let entry = { send = None; waiting = Hashtbl.create 4 } in
    Hashtbl.add t.relays relay_url entry;
    entry

let register_connection t ~relay_url ~send =
  let relay = normalize_url relay_url in
  Eio.Mutex.lock t.mutex;
  let entry = ensure_relay_entry t relay in
  entry.send <- Some send;
  (* On reconnect we keep outstanding waiters; they will receive either OK or timeout *)
  Eio.Mutex.unlock t.mutex

let remove_waiter entry event_id waiter =
  match Hashtbl.find_opt entry.waiting event_id with
  | None -> ()
  | Some waiters ->
    let remaining = List.filter (fun w -> w != waiter) waiters in
    if remaining = [] then
      Hashtbl.remove entry.waiting event_id
    else
      Hashtbl.replace entry.waiting event_id remaining

let notify_all entry event_id outcome =
  match Hashtbl.find_opt entry.waiting event_id with
  | None -> ()
  | Some waiters ->
    Hashtbl.remove entry.waiting event_id;
    List.iter
      (fun waiter ->
        (match waiter.outcome with
         | None -> waiter.outcome <- Some outcome
         | Some _ -> ());
        Eio.Condition.broadcast waiter.cond)
      waiters

let handle_ok t ~relay_url ~event_id ~ok ~message =
  let relay = normalize_url relay_url in
  Eio.Mutex.lock t.mutex;
  (match Hashtbl.find_opt t.relays relay with
   | None -> ()
   | Some entry ->
     let outcome = if ok then Ack message else Rejected message in
     notify_all entry event_id outcome);
  Eio.Mutex.unlock t.mutex

let handle_notice t ~relay_url ~message =
  let relay = normalize_url relay_url in
  Eio.Mutex.lock t.mutex;
  t.last_notice <- Some (relay, message);
  (* Leave outstanding waiters untouched; notice may not relate to specific event *)
  Eio.Mutex.unlock t.mutex

let detach_connection t ~relay_url ~reason =
  let relay = normalize_url relay_url in
  Eio.Mutex.lock t.mutex;
  (match Hashtbl.find_opt t.relays relay with
   | None -> ()
   | Some entry ->
     entry.send <- None;
      Hashtbl.iter
        (fun _event_id waiters ->
         List.iter
           (fun waiter ->
             (match waiter.outcome with
              | None -> waiter.outcome <- Some (Failed reason)
              | Some _ -> ());
             Eio.Condition.broadcast waiter.cond)
           waiters)
       entry.waiting;
     Hashtbl.clear entry.waiting);
  Eio.Mutex.unlock t.mutex

let wait_for_outcome t entry event_id waiter ~clock timeout =
  let rec loop () =
    match waiter.outcome with
    | Some outcome -> outcome
    | None ->
      (try
         Eio.Time.with_timeout_exn clock timeout (fun () -> Eio.Condition.await waiter.cond t.mutex)
       with
       | Eio.Time.Timeout ->
         waiter.outcome <- Some Timeout);
      loop ()
  in
  let outcome = loop () in
  remove_waiter entry event_id waiter;
  outcome

let publish_to_relay t ~clock ~timeout (event : Nostr_event.signed_event) relay_url =
  let relay = normalize_url relay_url in
  Eio.Mutex.lock t.mutex;
  let entry = ensure_relay_entry t relay in
  let send_opt = entry.send in
  (match send_opt with
   | None ->
     Eio.Mutex.unlock t.mutex;
     { relay; outcome = Failed "relay not connected" }
   | Some send ->
     let waiter = { cond = Eio.Condition.create (); outcome = None } in
     let existing = Hashtbl.find_opt entry.waiting event.id in
     let updated = waiter :: Option.value existing ~default:[] in
     Hashtbl.replace entry.waiting event.id updated;
     Eio.Mutex.unlock t.mutex;
     let send_result =
       try
         send event.message;
         None
       with
       | exn -> Some (Printexc.to_string exn)
     in
     (match send_result with
      | Some err ->
        Eio.Mutex.lock t.mutex;
        remove_waiter entry event.id waiter;
        Eio.Mutex.unlock t.mutex;
        { relay; outcome = Failed err }
      | None ->
        Eio.Mutex.lock t.mutex;
        let outcome = wait_for_outcome t entry event.id waiter ~clock timeout in
        Eio.Mutex.unlock t.mutex;
        { relay; outcome }))

let publish t ~clock ?timeout ~relays (event : Nostr_event.signed_event) =
  let timeout = Option.value timeout ~default:t.default_timeout in
  let deduped =
    relays
    |> List.map normalize_url
    |> List.sort_uniq String.compare
  in
  List.map (fun relay_url ->
    (* Create independent message copy for each relay to avoid stream corruption *)
    let event_copy = { 
      Nostr_event.json = event.json; 
      message = Bytes.to_string (Bytes.of_string event.message); 
      id = event.id 
    } in
    publish_to_relay t ~clock ~timeout event_copy relay_url
  ) deduped

let last_notice t =
  Eio.Mutex.lock t.mutex;
  let notice = t.last_notice in
  Eio.Mutex.unlock t.mutex;
  notice
