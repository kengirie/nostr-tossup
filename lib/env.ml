open Stdlib

let strip_quotes s =
  let len = String.length s in
  if len >= 2 then
    match (s.[0], s.[len - 1]) with
    | ('"', '"') | ('\'', '\'') -> String.sub s 1 (len - 2)
    | _ -> s
  else
    s

let parse_line line =
  let trimmed = String.trim line in
  if trimmed = "" || trimmed.[0] = '#' then
    None
  else
    match String.index_opt trimmed '=' with
    | None -> None
    | Some idx ->
      let key = String.sub trimmed 0 idx |> String.trim in
      if key = "" then None
      else
        let raw_value =
          String.sub trimmed (idx + 1) (String.length trimmed - idx - 1)
          |> String.trim
        in
        let value = strip_quotes raw_value in
        Some (key, value)

let load_dotenv ?(filename = ".env") () =
  if Sys.file_exists filename then (
    let ic = open_in filename in
    Fun.protect
      ~finally:(fun () -> close_in_noerr ic)
      (fun () ->
        let rec loop () =
          match input_line ic with
          | line ->
            (match parse_line line with
             | Some (key, value) -> Unix.putenv key value
             | None -> ());
            loop ()
          | exception End_of_file -> ()
        in
        loop ()))

let load_nsec_secret ?dotenv_path () =
  (match dotenv_path with
   | Some path -> load_dotenv ~filename:path ()
   | None -> load_dotenv ());
  let env_var = "NOSTR_NSEC" in
  match Sys.getenv_opt env_var with
  | Some value when value <> "" -> Nip19.decode_nsec value
  | Some _ -> Error (Printf.sprintf "%s is set but empty" env_var)
  | None -> Error (Printf.sprintf "%s is not set" env_var)
