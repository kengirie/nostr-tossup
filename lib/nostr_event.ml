type signed_event = {
  json : Yojson.Safe.t;
  message : string;
  id : string;
}

module Hex = struct
  let hex_chars = "0123456789abcdef"

  let bytes_to_hex bytes =
    let len = Bytes.length bytes in
    let buf = Bytes.create (len * 2) in
    for i = 0 to len - 1 do
      let v = int_of_char (Bytes.get bytes i) land 0xff in
      Bytes.set buf (2 * i) hex_chars.[v lsr 4];
      Bytes.set buf ((2 * i) + 1) hex_chars.[v land 0x0f]
    done;
    Bytes.unsafe_to_string buf
end

let json_of_tags tags =
  `List
    (List.map
       (fun tag -> `List (List.map (fun entry -> `String entry) tag))
       tags)

let serialize_event ~pubkey_hex ~created_at ~kind ~tags ~content =
  `List
    [ `Int 0;
      `String pubkey_hex;
      `Int created_at;
      `Int kind;
      json_of_tags tags;
      `String content
    ]

let compute_event_hash serialized =
  let serialized_string = Yojson.Safe.to_string serialized in
  let hash_bin = Sha256.string serialized_string |> Sha256.to_bin in
  (serialized_string, hash_bin)

let finalize_event_json ~pubkey_hex ~created_at ~kind ~tags ~content ~event_id ~signature_hex =
  `Assoc
    [ ("id", `String event_id);
      ("pubkey", `String pubkey_hex);
      ("created_at", `Int created_at);
      ("kind", `Int kind);
      ("tags", json_of_tags tags);
      ("content", `String content);
      ("sig", `String signature_hex)
    ]

let kind1_event_message ~content ~tags ~keypair =
  let created_at = int_of_float (Unix.gettimeofday ()) in
  let pubkey_bytes = Bip340.public_key keypair in
  let pubkey_hex = Hex.bytes_to_hex pubkey_bytes in
  let serialized = serialize_event ~pubkey_hex ~created_at ~kind:1 ~tags ~content in
  let _, hash_bin = compute_event_hash serialized in
  if String.length hash_bin <> 32 then
    invalid_arg "kind1_event_message: expected 32-byte hash";
  let event_id = Hex.bytes_to_hex (Bytes.of_string hash_bin) in
  let signature_bytes = Bip340.sign_prehashed ~keypair hash_bin in
  let signature_hex = Hex.bytes_to_hex signature_bytes in
  let event_json =
    finalize_event_json
      ~pubkey_hex
      ~created_at
      ~kind:1
      ~tags
      ~content
      ~event_id
      ~signature_hex
  in
  let event_message =
    `List [ `String "EVENT"; event_json ] |> Yojson.Safe.to_string
  in
  { json = event_json; message = event_message; id = event_id }
