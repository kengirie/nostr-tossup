open Ctypes
open Foreign

(* Types exposed by the libsecp256k1 bindings *)

type secp256k1_context = unit ptr

let secp256k1_context : secp256k1_context typ = ptr void

let secp256k1_keypair : unit abstract typ =
  abstract ~name:"secp256k1_keypair" ~size:96 ~alignment:0

let secp256k1_xonly_pubkey : unit abstract typ =
  abstract ~name:"secp256k1_xonly_pubkey" ~size:64 ~alignment:0

(* Foreign function imports *)

let secp256k1_context_create =
  foreign "secp256k1_context_create" (uint @-> returning secp256k1_context)

let secp256k1_keypair_create =
  foreign
    "secp256k1_keypair_create"
    (secp256k1_context @-> ptr secp256k1_keypair @-> ptr char @-> returning int)

let secp256k1_keypair_xonly_pub =
  foreign
    "secp256k1_keypair_xonly_pub"
    (secp256k1_context
     @-> ptr secp256k1_xonly_pubkey
     @-> ptr int
     @-> ptr secp256k1_keypair
     @-> returning int)

let secp256k1_xonly_pubkey_serialize =
  foreign
    "secp256k1_xonly_pubkey_serialize"
    (secp256k1_context @-> ptr char @-> ptr secp256k1_xonly_pubkey @-> returning int)

let secp256k1_xonly_pubkey_parse =
  foreign
    "secp256k1_xonly_pubkey_parse"
    (secp256k1_context @-> ptr secp256k1_xonly_pubkey @-> ptr char @-> returning int)

let secp256k1_schnorrsig_sign32 =
  foreign
    "secp256k1_schnorrsig_sign32"
    (secp256k1_context
     @-> ptr char
     @-> ptr char
     @-> ptr secp256k1_keypair
     @-> ptr char
     @-> returning int)

let secp256k1_schnorrsig_verify =
  foreign
    "secp256k1_schnorrsig_verify"
    (secp256k1_context
     @-> ptr char
     @-> ptr char
     @-> size_t
     @-> ptr secp256k1_xonly_pubkey
     @-> returning int)

(* Global signing context. Flags enable signing and verification. *)

let context =
  let sign_flag = 1 lsl 0 in
  let verify_flag = 1 lsl 8 in
  let extrakeys_flag = 1 lsl 9 in
  let flags = Unsigned.UInt.of_int (sign_flag lor verify_flag lor extrakeys_flag) in
  secp256k1_context_create flags

(* Helpers *)

let ensure_ok fn_name ret_code =
  if ret_code <> 1 then
    invalid_arg (Printf.sprintf "%s returned %d" fn_name ret_code)

(* Key utilities *)

type keypair = unit ptr

type xonly_pubkey = unit ptr

let load_secret secret_key =
  if Bytes.length secret_key <> 32 then
    invalid_arg "Bip340.load_secret: secret key must be 32 bytes";
  let sk_ptr = allocate_n char ~count:32 in
  Bytes.iteri (fun i b -> sk_ptr +@ i <-@ b) secret_key;
  let keypair_ptr = allocate_n secp256k1_keypair ~count:1 in
  let keypair = keypair_ptr +@ 0 in
  let ret = secp256k1_keypair_create context keypair sk_ptr in
  ensure_ok "secp256k1_keypair_create" ret;
  keypair

let public_key keypair =
  let xonly_ptr = allocate_n secp256k1_xonly_pubkey ~count:1 in
  let xonly = xonly_ptr +@ 0 in
  let parity = allocate int 0 in
  let ret = secp256k1_keypair_xonly_pub context xonly parity keypair in
  ensure_ok "secp256k1_keypair_xonly_pub" ret;
  let output = allocate_n char ~count:32 in
  let ret = secp256k1_xonly_pubkey_serialize context output xonly in
  ensure_ok "secp256k1_xonly_pubkey_serialize" ret;
  Bytes.init 32 (fun i -> !@(output +@ i))

(* Signing helpers *)

let sign_prehashed ~keypair hash32 =
  if String.length hash32 <> 32 then
    invalid_arg "Bip340.sign_prehashed: message hash must be 32 bytes";
  let msg32 = allocate_n char ~count:32 in
  String.iteri (fun i c -> msg32 +@ i <-@ c) hash32;
  let signature = allocate_n char ~count:64 in
  let aux_rand = allocate_n char ~count:32 in
  for i = 0 to 31 do
    aux_rand +@ i <-@ Char.chr 0
  done;
  let ret = secp256k1_schnorrsig_sign32 context signature msg32 keypair aux_rand in
  ensure_ok "secp256k1_schnorrsig_sign32" ret;
  Bytes.init 64 (fun i -> !@(signature +@ i))

let sign ~keypair message =
  let msg_hash = Sha256.string message |> Sha256.to_bin in
  sign_prehashed ~keypair msg_hash

let verify_prehashed ~pubkey hash32 signature =
  if String.length hash32 <> 32 then
    invalid_arg "Bip340.verify_prehashed: message hash must be 32 bytes";
  if Bytes.length pubkey <> 32 then
    invalid_arg "Bip340.verify_prehashed: pubkey must be 32 bytes";
  if Bytes.length signature <> 64 then
    invalid_arg "Bip340.verify_prehashed: signature must be 64 bytes";
  let msg32 = allocate_n char ~count:32 in
  String.iteri (fun i c -> msg32 +@ i <-@ c) hash32;
  let sig64 = allocate_n char ~count:64 in
  Bytes.iteri (fun i c -> sig64 +@ i <-@ c) signature;
  let xonly_ptr = allocate_n secp256k1_xonly_pubkey ~count:1 in
  let xonly = xonly_ptr +@ 0 in
  let pk32 = allocate_n char ~count:32 in
  Bytes.iteri (fun i c -> pk32 +@ i <-@ c) pubkey;
  let ret = secp256k1_xonly_pubkey_parse context xonly pk32 in
  ensure_ok "secp256k1_xonly_pubkey_parse" ret;
  let ok =
    secp256k1_schnorrsig_verify
      context
      sig64
      msg32
      (Unsigned.Size_t.of_int 32)
      xonly
  in
  ok = 1

let verify ~pubkey message signature =
  let msg_hash = Sha256.string message |> Sha256.to_bin in
  verify_prehashed ~pubkey msg_hash signature
