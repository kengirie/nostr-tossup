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
  (* "https://x.kojira.io"; *)
  (* "https://relay.primal.net/";
  "https://nos.lol";
  "https://relay.nostr.band"; *)
]

let periodic_relays = [
  "https://nos.lol";
  "https://nostr.mom";
  "https://relay.damus.io";
]

let all_relays =
  List.sort_uniq String.compare (publish_relays @ subscribe_relays @ periodic_relays)

(* Reconnection settings *)
let reconnect_delay = 60.0

let eose_timeout = 10.0        (* Wait 10 seconds for EOSE response *)
