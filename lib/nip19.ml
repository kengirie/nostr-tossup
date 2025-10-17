type error = string

let charset = "qpzry9x8gf2tvdw0s3jn54khce6mua7l"

let rev_table =
  let table = Array.make 128 (-1) in
  String.iteri (fun idx ch -> table.(Char.code ch) <- idx) charset;
  table

let polymod values =
  let generators = [| 0x3b6a57b2; 0x26508e6d; 0x1ea119fa; 0x3d4233dd; 0x2a1462b3 |] in
  let rec apply_generators top chk idx =
    if idx > 4 then chk
    else
      let chk' =
        if ((top lsr idx) land 1) = 1 then chk lxor generators.(idx) else chk
      in
      apply_generators top chk' (idx + 1)
  in
  let rec loop chk = function
    | [] -> chk
    | value :: rest ->
      let top = chk lsr 25 in
      let chk' = ((chk land 0x1ffffff) lsl 5) lxor value in
      let chk'' = apply_generators top chk' 0 in
      loop chk'' rest
  in
  loop 1 values

let hrp_expand hrp =
  let len = String.length hrp in
  let rec loop idx high low =
    if idx = len then List.rev high @ (0 :: List.rev low)
    else
      let code = Char.code hrp.[idx] in
      loop (idx + 1) ((code lsr 5) :: high) ((code land 31) :: low)
  in
  loop 0 [] []

let verify_checksum hrp data =
  polymod (hrp_expand hrp @ data) = 1

let create_checksum hrp data =
  let values = hrp_expand hrp @ data @ [0; 0; 0; 0; 0; 0] in
  let pm = (polymod values) lxor 1 in
  List.init 6 (fun i -> (pm lsr (5 * (5 - i))) land 31)

let convertbits ~data ~frombits ~tobits ~pad =
  let maxv = (1 lsl tobits) - 1 in
  let rec finalize bits acc result =
    if pad then
      let result =
        if bits > 0 then
          let value = (acc lsl (tobits - bits)) land maxv in
          value :: result
        else
          result
      in
      Ok (List.rev result)
    else if bits >= frombits then
      Error "convertbits: excess padding"
    else if ((acc lsl (tobits - bits)) land maxv) <> 0 then
      Error "convertbits: non-zero padding"
    else
      Ok (List.rev result)
  and extract acc bits result =
    if bits >= tobits then
      let bits = bits - tobits in
      let value = (acc lsr bits) land maxv in
      extract acc bits (value :: result)
    else
      (acc, bits, result)
  and process data acc bits result =
    match data with
    | [] -> finalize bits acc result
    | value :: rest ->
      if value < 0 || value >= (1 lsl frombits) then
        Error "convertbits: invalid value"
      else
        let acc = (acc lsl frombits) lor value in
        let bits = bits + frombits in
        let acc, bits, result = extract acc bits result in
        process rest acc bits result
  in
  process data 0 0 []

let decode_bech32 str =
  let lowercase = String.lowercase_ascii str in
  match String.rindex_opt lowercase '1' with
  | None -> Error "Bech32: missing separator"
  | Some pos when pos < 1 -> Error "Bech32: invalid HRP"
  | Some pos when pos + 7 > String.length lowercase ->
    Error "Bech32: data part too short"
  | Some pos ->
    let hrp = String.sub lowercase 0 pos in
    let data_part =
      String.sub lowercase (pos + 1) (String.length lowercase - pos - 1)
    in
    if String.exists (fun c -> Char.code c < 33 || Char.code c > 126) lowercase then
      Error "Bech32: invalid characters"
    else
      let data_values =
        let len = String.length data_part in
        let rec loop idx acc =
          if idx = len then
            Ok (List.rev acc)
          else
            let ch = data_part.[idx] in
            let code = Char.code ch in
            if code >= Array.length rev_table then
              Error "Bech32: invalid character"
            else
              let value = rev_table.(code) in
              if value = -1 then Error "Bech32: invalid character"
              else loop (idx + 1) (value :: acc)
        in
        loop 0 []
      in
      match data_values with
      | Error _ as e -> e
      | Ok data ->
        if not (verify_checksum hrp data) then
          Error "Bech32: checksum mismatch"
        else
          let payload_len = List.length data - 6 in
          if payload_len <= 0 then Error "Bech32: missing payload"
          else
            let payload =
              let rec take n xs =
                match (n, xs) with
                | 0, _ | _, [] -> []
                | n, x :: xs -> x :: take (n - 1) xs
              in
              take payload_len data
            in
            Ok (hrp, payload)

let encode_bech32 ~hrp data =
  let data_with_checksum = data @ create_checksum hrp data in
  let buffer = Buffer.create (String.length hrp + 1 + List.length data_with_checksum) in
  Buffer.add_string buffer hrp;
  Buffer.add_char buffer '1';
  List.iter
    (fun v -> Buffer.add_char buffer charset.[v])
    data_with_checksum;
  Buffer.contents buffer

let decode_nsec str =
  match decode_bech32 str with
  | Error _ as e -> e
  | Ok (hrp, payload) ->
    if hrp <> "nsec" then
      Error "NIP-19: HRP is not nsec"
    else
      match convertbits ~data:payload ~frombits:5 ~tobits:8 ~pad:false with
      | Error _ as e -> e
      | Ok bytes ->
        let len = List.length bytes in
        if len <> 32 then
          Error "NIP-19: invalid secret key length"
        else
          let secret = Bytes.create len in
          List.iteri (fun idx value -> Bytes.set secret idx (Char.chr value)) bytes;
          Ok secret

let encode_bytes ~hrp bytes =
  let len = Bytes.length bytes in
  if len <> 32 then invalid_arg (Printf.sprintf "%s expects 32-byte input" hrp);
  let byte_list =
    let rec loop idx acc =
      if idx < 0 then acc
      else loop (idx - 1) (Char.code (Bytes.get bytes idx) :: acc)
    in
    loop (len - 1) []
  in
  match convertbits ~data:byte_list ~frombits:8 ~tobits:5 ~pad:true with
  | Error msg -> invalid_arg msg
  | Ok data -> encode_bech32 ~hrp data

let encode_nsec secret = encode_bytes ~hrp:"nsec" secret

let encode_npub public_key = encode_bytes ~hrp:"npub" public_key
