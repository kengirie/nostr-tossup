(* Nostr relay configuration *)
let publish_relays = [
   "https://nostr.mom";
  "https://nostr.bitcoiner.social";
  "https://nos.lol";
  (*"https://relay.primal.net/";
  "https://relay.damus.io"; *)
]

let subscribe_relays = [
  "https://relay.damus.io";
  "https://nos.lol";
  "https://r.kojira.io";
]

let all_relays =
  List.sort_uniq String.compare (publish_relays @ subscribe_relays)

(* Reconnection settings *)
let reconnect_delay = 20.0

(* Keep-alive settings *)
let keepalive_interval = 30.0  (* Send keepalive REQ every 30 seconds *)
let eose_timeout = 10.0        (* Wait 10 seconds for EOSE response *)
