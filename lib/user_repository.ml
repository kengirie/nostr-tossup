open Caqti_request.Infix
open Caqti_type

module Queries = struct
  let insert_user =
    (t3 string string int ->. unit)
      "INSERT OR IGNORE INTO users (pubkey, registration_date, existing_user)\n       VALUES (?, ?, ?)"
end

let insert_if_missing (module Db : Caqti_eio.CONNECTION) ~npub ~registration_date ~existing_user =
  Db.exec Queries.insert_user (npub, registration_date, existing_user)
