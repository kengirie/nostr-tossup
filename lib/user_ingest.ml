open Eio.Std

let hex_value c =
  match c with
  | '0' .. '9' -> Some (Char.code c - Char.code '0')
  | 'a' .. 'f' -> Some (10 + Char.code c - Char.code 'a')
  | 'A' .. 'F' -> Some (10 + Char.code c - Char.code 'A')
  | _ -> None

let hex_to_bytes hex =
  let len = String.length hex in
  if len mod 2 <> 0 then Error "hex string must have even length"
  else
    let bytes = Bytes.create (len / 2) in
    let rec loop idx =
      if idx >= len then Ok bytes
      else
        let high = hex_value hex.[idx] in
        let low = hex_value hex.[idx + 1] in
        match (high, low) with
        | Some hi, Some lo ->
          Bytes.set bytes (idx / 2) (Char.chr ((hi lsl 4) lor lo));
          loop (idx + 2)
        | _ -> Error "invalid hex character"
    in
    loop 0

let today_string () =
  let open Unix in
  let tm = localtime (time ()) in
  Printf.sprintf "%04d-%02d-%02d"
    (tm.tm_year + 1900)
    (tm.tm_mon + 1)
    tm.tm_mday

let with_lock mutex f =
  Eio.Mutex.lock mutex;
  Fun.protect ~finally:(fun () -> Eio.Mutex.unlock mutex) f

let start ~sw ~clock ~stdenv ?uri ?(interval = 60.) () =
  let pending = Hashtbl.create 128 in
  let mutex = Eio.Mutex.create () in

  let process_batch npubs =
    if npubs = [] then ()
    else
      let registration_date = today_string () in
      Database.with_connection ?uri ~sw ~stdenv @@ fun conn ->
      List.iter
        (fun npub ->
          try
            User_repository.insert_if_missing conn
              ~npub
              ~registration_date
              ~existing_user:0
          with
          | Caqti_error.Exn err ->
            traceln "Failed to insert user %s: %a" npub Caqti_error.pp err)
        npubs
  in

  let rec loop () =
    Eio.Time.sleep clock interval;
    let batch =
      with_lock mutex (fun () ->
          let collected = Hashtbl.fold (fun npub () acc -> npub :: acc) pending [] in
          Hashtbl.reset pending;
          collected)
    in
    (if batch <> [] then
       try
         process_batch batch
       with
       | Caqti_error.Exn err ->
         traceln "Database batch failed: %a" Caqti_error.pp err
       | exn ->
         traceln "Database batch failed: %s" (Printexc.to_string exn));
    loop ()
  in

  Fiber.fork ~sw (fun () -> loop ());

  fun pubkey_hex _content ->
    match hex_to_bytes pubkey_hex with
    | Error msg ->
      traceln "Ignoring pubkey %s: %s" pubkey_hex msg
    | Ok bytes ->
      (try
         let npub = Nip19.encode_npub bytes in
         with_lock mutex (fun () -> Hashtbl.replace pending npub ())
       with
       | Invalid_argument msg ->
         traceln "Failed to encode npub for %s: %s" pubkey_hex msg)
