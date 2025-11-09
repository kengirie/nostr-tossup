open Caqti_request.Infix
open Caqti_type

module Queries = struct
  let insert_user =
    (t3 string string int ->. unit)
      "INSERT OR IGNORE INTO users (pubkey, registration_date, existing_user)\n       VALUES (?, ?, ?)"

  let recent_new_users =
    (unit ->* string)
      "SELECT pubkey\n       FROM users\n       WHERE existing_user = 0\n         AND is_bot = 0\n         AND registration_date >= datetime('now', '-1 month')\n       ORDER BY registration_date DESC"

  let next_unclassified =
    (unit ->? string)
      "SELECT pubkey\n       FROM users\n       WHERE existing_user = 0\n         AND is_bot = 2\n       ORDER BY RANDOM()\n       LIMIT 1"

  let update_is_bot =
    (t2 int string ->. unit)
      "UPDATE users\n       SET is_bot = ?\n       WHERE pubkey = ?"

  let update_existing_user =
    (t2 int string ->. unit)
      "UPDATE users\n       SET existing_user = ?\n       WHERE pubkey = ?"

  let next_new_user =
    (unit ->? string)
      "SELECT pubkey\n       FROM users\n       WHERE existing_user = 0\n       ORDER BY RANDOM()\n       LIMIT 1"
end

let insert_if_missing (module Db : Caqti_eio.CONNECTION) ~npub ~registration_date ~existing_user =
  Db.exec Queries.insert_user (npub, registration_date, existing_user)

let list_recent_new_users (module Db : Caqti_eio.CONNECTION) =
  Db.collect_list Queries.recent_new_users ()

let next_unclassified_user (module Db : Caqti_eio.CONNECTION) =
  Db.find_opt Queries.next_unclassified ()

let update_is_bot (module Db : Caqti_eio.CONNECTION) ~npub ~is_bot =
  Db.exec Queries.update_is_bot (is_bot, npub)

let update_existing_user (module Db : Caqti_eio.CONNECTION) ~npub ~existing_user =
  Db.exec Queries.update_existing_user (existing_user, npub)

let next_new_user (module Db : Caqti_eio.CONNECTION) =
  Db.find_opt Queries.next_new_user ()
