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
  Unix.putenv "NOSTR_NSEC" ""
