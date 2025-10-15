(lang dune 3.20)

(name nostr-tossup)

(generate_opam_files true)

(source
 (github username/reponame))

(authors "Author Name <author@example.com>")

(maintainers "Maintainer Name <maintainer@example.com>")

(license LICENSE)

(documentation https://url/to/documentation)

(package
 (name nostr-tossup)
 (synopsis "A short synopsis")
 (description "A longer description")
 (depends
  ocaml
  eio
  piaf
  yojson
  ctypes
  ctypes-foreign
  caqti
  caqti-driver-sqlite3
  caqti-eio
  uri
  sha)
 (tags
  ("add topics" "to describe" your project)))

; See the complete stanza docs at https://dune.readthedocs.io/en/stable/reference/dune-project/index.html
