open Caqti_request.Infix
open Caqti_type

module Queries = struct
  let insert_user =
    (t3 string string int ->. unit)
      "INSERT OR IGNORE INTO users (pubkey, registration_date, existing_user)\n       VALUES (?, ?, ?)"

  let recent_new_users =
    (unit ->* string)
      "SELECT pubkey\n       FROM users\n       WHERE existing_user = 0\n         AND registration_date >= datetime('now', '-1 month')\n       ORDER BY registration_date DESC"
end

let insert_if_missing (module Db : Caqti_eio.CONNECTION) ~npub ~registration_date ~existing_user =
  Db.exec Queries.insert_user (npub, registration_date, existing_user)

let list_recent_new_users (module Db : Caqti_eio.CONNECTION) =
  Db.collect_list Queries.recent_new_users ()
