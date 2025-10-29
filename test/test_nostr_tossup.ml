let () =
  let open Nostr_tossup.Bip340 in
  let secret = Bytes.make 32 '\001' in
  let keypair = load_secret secret in
  let pubkey = public_key keypair in
  assert (Bytes.length pubkey = 32);
  let message = "nostr bip340 test" in
  let signature = sign ~keypair message in
  assert (Bytes.length signature = 64);
  assert (verify ~pubkey message signature);
  assert (not (verify ~pubkey "tampered" signature));

  let encoded = Nostr_tossup.Nip19.encode_nsec secret in
  (match Nostr_tossup.Nip19.decode_nsec encoded with
   | Ok decoded -> assert (decoded = secret)
   | Error msg -> failwith msg);

  let dotenv = Filename.temp_file "nostr" ".env" in
  let oc = open_out dotenv in
  output_string oc ("NOSTR_NSEC=" ^ encoded ^ "\n");
  close_out oc;
  (* Ensure environment lookup uses .env *)
  (match Nostr_tossup.Env.load_nsec_secret ~dotenv_path:dotenv () with
   | Ok from_env -> assert (from_env = secret)
   | Error msg -> failwith msg);
  Sys.remove dotenv;
  Unix.putenv "NOSTR_NSEC" "";

  let pubkey_hex = Nostr_tossup.Nostr_event.Hex.bytes_to_hex pubkey in
  let tags = [ [ "p"; "relay" ]; [ "client"; "nostr-tossup" ] ] in
  let created_at = 1_700_000_000 in
  let serialized_event =
    Nostr_tossup.Nostr_event.serialize_event
      ~pubkey_hex
      ~created_at
      ~kind:1
      ~tags
      ~content:"hello nostr"
  in
  let expected_serialized =
    `List
      [ `Int 0;
        `String pubkey_hex;
        `Int created_at;
        `Int 1;
        `List
          [ `List [ `String "p"; `String "relay" ];
            `List [ `String "client"; `String "nostr-tossup" ]
          ];
        `String "hello nostr"
      ]
  in
  assert (serialized_event = expected_serialized);

  let serialized_string, hash_bin =
    Nostr_tossup.Nostr_event.compute_event_hash serialized_event
  in
  assert (serialized_string = Yojson.Safe.to_string serialized_event);
  let expected_hash = Sha256.string serialized_string |> Sha256.to_bin in
  assert (hash_bin = expected_hash);
  assert (String.length hash_bin = 32);

  let event_id =
    Nostr_tossup.Nostr_event.Hex.bytes_to_hex (Bytes.of_string hash_bin)
  in
  let signature_bytes = Nostr_tossup.Bip340.sign_prehashed ~keypair hash_bin in
  let signature_hex =
    Nostr_tossup.Nostr_event.Hex.bytes_to_hex signature_bytes
  in
  let event_json =
    Nostr_tossup.Nostr_event.finalize_event_json
      ~pubkey_hex
      ~created_at
      ~kind:1
      ~tags
      ~content:"hello nostr"
      ~event_id
      ~signature_hex
  in
  let open Yojson.Safe.Util in
  let id_from_json = member "id" event_json |> to_string in
  let pubkey_from_json = member "pubkey" event_json |> to_string in
  let created_from_json = member "created_at" event_json |> to_int in
  let kind_from_json = member "kind" event_json |> to_int in
  let signature_from_json = member "sig" event_json |> to_string in
  let content_from_json = member "content" event_json |> to_string in
  let decoded_tags =
    member "tags" event_json
    |> to_list
    |> List.map (fun entry -> entry |> to_list |> List.map to_string)
  in
  assert (event_id = id_from_json);
  assert (pubkey_hex = pubkey_from_json);
  assert (created_at = created_from_json);
  assert (kind_from_json = 1);
  assert (signature_hex = signature_from_json);
  assert ("hello nostr" = content_from_json);
  assert (decoded_tags = tags);

  let hex_to_bytes hex =
    let hex_value = function
      | '0' .. '9' as c -> Char.code c - Char.code '0'
      | 'a' .. 'f' as c -> 10 + Char.code c - Char.code 'a'
      | 'A' .. 'F' as c -> 10 + Char.code c - Char.code 'A'
      | c -> failwith (Printf.sprintf "invalid hex char: %c" c)
    in
    let len = String.length hex in
    assert (len mod 2 = 0);
    let bytes = Bytes.create (len / 2) in
    for i = 0 to (len / 2) - 1 do
      let hi = hex_value hex.[2 * i] in
      let lo = hex_value hex.[(2 * i) + 1] in
      Bytes.set bytes i (Char.chr ((hi lsl 4) lor lo))
    done;
    bytes
  in

  let npub = Nostr_tossup.Nip19.encode_npub pubkey in
  (match Nostr_tossup.Nostr_utils.npub_to_hex npub with
   | Ok hex -> assert (Bytes.equal (hex_to_bytes hex) pubkey)
   | Error msg -> failwith msg);
  (match Nostr_tossup.Nostr_utils.npub_to_hex "npub1invalid" with
   | Ok _ -> assert false
   | Error _ -> ());

  let normalized =
    Nostr_tossup.Nostr_utils.normalize_url " WSS://Example.com:443/deck "
  in
  assert (normalized = "wss://example.com:443/deck");
  let normalized_root =
    Nostr_tossup.Nostr_utils.normalize_url "wss://example.com"
  in
  assert (normalized_root = "wss://example.com/");

  assert (not (Nostr_tossup.Text_utils.contains_kana "nostr"));
  assert (Nostr_tossup.Text_utils.contains_kana "nostrかな" );
  assert (Nostr_tossup.Text_utils.contains_kana "カタカナ mixed" )
