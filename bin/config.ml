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

(* Subscription settings *)
let subscription_id = "my_sub"