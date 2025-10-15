open Eio.Std

let default_uri = "sqlite3:./data.sqlite3"

let resolve_uri = function
  | Some uri -> Uri.of_string uri
  | None -> Uri.of_string default_uri

let with_connection ?uri ~sw ~stdenv f =
  let uri = resolve_uri uri in
  let conn =
    Caqti_eio_unix.connect ~sw ~stdenv uri |> Caqti_eio.or_fail
  in
  Fun.protect ~finally:(fun () ->
      let (module Db : Caqti_eio.CONNECTION) = conn in
      Db.disconnect ())
    (fun () -> f conn)

module Sql_file = struct
  let starts_with ~prefix s =
    let prefix_len = String.length prefix in
    String.length s >= prefix_len
    && String.sub s 0 prefix_len = prefix

  let sanitize_lines content =
    content
    |> String.split_on_char '\n'
    |> List.filter_map (fun line ->
        let trimmed = String.trim line in
        if trimmed = "" then
          None
        else if starts_with ~prefix:"--" trimmed then
          None
        else
          Some line)

  let read_file path =
    let ic = open_in path in
    Fun.protect
      ~finally:(fun () -> close_in_noerr ic)
      (fun () -> really_input_string ic (in_channel_length ic))

  let statements_of_file path =
    read_file path
    |> sanitize_lines
    |> String.concat "\n"
    |> String.split_on_char ';'
    |> List.filter_map (fun statement ->
        let trimmed = String.trim statement in
        if trimmed = "" then None else Some trimmed)
end

type exec_outcome =
  [ `Applied
  | `Skipped_integrity_violation ]

let exec_unit_statement conn sql : exec_outcome =
  let (module Db : Caqti_eio.CONNECTION) = conn in
  let open Caqti_request.Infix in
  let request = (Caqti_type.unit ->. Caqti_type.unit) sql in
  match Db.exec request () with
  | Ok () -> `Applied
  | Error ((`Response_failed _ | `Request_failed _) as err) ->
    (match Caqti_error.cause err with
     | #Caqti_error.integrity_constraint_violation as cause ->
       traceln "Skipping statement (%s): %s"
         (Caqti_error.show_cause cause) sql;
       `Skipped_integrity_violation
     | _ -> raise (Caqti_error.Exn err))
  | Error err -> raise (Caqti_error.Exn err)

let apply_sql_file ?uri ~sw ~stdenv path =
  let statements = Sql_file.statements_of_file path in
  match statements with
  | [] -> traceln "No SQL statements found in %s" path
  | stmts ->
    with_connection ?uri ~sw ~stdenv @@ fun conn ->
    let applied, skipped =
      List.fold_left
        (fun (applied, skipped) statement ->
           match exec_unit_statement conn statement with
           | `Applied -> (applied + 1, skipped)
           | `Skipped_integrity_violation -> (applied, skipped + 1))
        (0, 0)
        stmts
    in
    traceln "Applied %d statements (skipped %d) from %s"
      applied skipped path

let ensure_schema ?uri ?(schema_path = "sql/schema.sql") ~sw ~stdenv () =
  apply_sql_file ?uri ~sw ~stdenv schema_path
