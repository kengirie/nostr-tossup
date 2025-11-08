let decode_utf8_code str idx =
  let len = String.length str in
  let byte i = Char.code str.[i] in
  if idx >= len then None
  else
    let b0 = byte idx in
    if b0 land 0x80 = 0 then Some (b0, idx + 1)
    else if (b0 land 0xE0) = 0xC0 && idx + 1 < len then
      let b1 = byte (idx + 1) in
      if b1 land 0xC0 <> 0x80 then Some (0xFFFD, idx + 1)
      else
        let code = ((b0 land 0x1F) lsl 6) lor (b1 land 0x3F) in
        Some (code, idx + 2)
    else if (b0 land 0xF0) = 0xE0 && idx + 2 < len then
      let b1 = byte (idx + 1) in
      let b2 = byte (idx + 2) in
      if b1 land 0xC0 <> 0x80 || b2 land 0xC0 <> 0x80 then Some (0xFFFD, idx + 1)
      else
        let code =
          ((b0 land 0x0F) lsl 12)
          lor ((b1 land 0x3F) lsl 6)
          lor (b2 land 0x3F)
        in
        Some (code, idx + 3)
    else if (b0 land 0xF8) = 0xF0 && idx + 3 < len then
      let b1 = byte (idx + 1) in
      let b2 = byte (idx + 2) in
      let b3 = byte (idx + 3) in
      if b1 land 0xC0 <> 0x80 || b2 land 0xC0 <> 0x80 || b3 land 0xC0 <> 0x80 then
        Some (0xFFFD, idx + 1)
      else
        let code =
          ((b0 land 0x07) lsl 18)
          lor ((b1 land 0x3F) lsl 12)
          lor ((b2 land 0x3F) lsl 6)
          lor (b3 land 0x3F)
        in
        Some (code, idx + 4)
    else
      Some (0xFFFD, idx + 1)

let is_kana code =
  (code >= 0x3040 && code <= 0x309F)
  || (code >= 0x30A0 && code <= 0x30FF && code <> 0x30C4 && code <> 0x30CE)

let contains_kana str =
  let rec loop idx =
    match decode_utf8_code str idx with
    | None -> false
    | Some (code, next_idx) ->
      if is_kana code then true else loop next_idx
  in
  loop 0
