# Repository Guidelines

## Project Structure & Module Organization
- `bin/main.ml` is the entrypoint; it seeds SQLite from `sql/backup.sql` and launches relay connections.
- `lib/` holds core modules (`nostr_connection.ml`, `bip340.ml`, `env.ml`, etc.); keep shared logic there.
- `sql/schema.sql` and `sql/backup.sql` define database layout and bootstrap data; update both together when schema changes.
- `test/test_nostr_tossup.ml` exercises signing, NIP-19 helpers, and `.env` loading; mirror its patterns when adding coverage.
- `docs/` is for focused references (e.g., `bip340.md`); add design notes or protocol research here.

## Build, Test, and Development Commands
- `opam install . --deps-only` installs OCaml dependencies for a fresh checkout.
- `dune build` compiles the library, binary, and SQL helpers; run before opening a PR.
- `dune exec bin/main.exe` runs the relay client using the current `.env` and `data.sqlite3`.
- `dune test` (alias of `dune runtest`) runs the current unit suite; use `--watch` while iterating.
- `dune clean` clears `_build/` artifacts if you hit stale module issues.

## Coding Style & Naming Conventions
- Use two-space indentation and `let` bindings formatted by `ocamlformat`; run `dune fmt` before committing.
- Modules are `UpperCamelCase` (e.g., `Nostr_connection`), functions and values use `snake_case`.
- Keep side-effectful helpers in dedicated modules (e.g., networking in `nostr_connection.ml`) and expose pure helpers via `.mli` files when adding interfaces.
- Prefer pattern matching over chained conditionals and add succinct OCaml comments `(* like this *)` for non-obvious flow.

## Testing Guidelines
- Extend `test/test_nostr_tossup.ml` or split into additional files under `test/` when scenarios grow.
- Follow the existing `assert`-based style; document complex fixtures with helper functions.
- Aim to cover new serialization, database, and crypto branches; mock environment-dependent pieces with temp files as shown.
- Run `dune test --force` after touching SQL or async code to ensure clean reruns.

## Commit & Pull Request Guidelines
- Follow Conventional Commits (`feat:`, `fix:`, `refactor:`) as seen in `git log` for clear history.
- Each commit should build and pass tests locally; mention `dune build && dune test` in the PR description.
- Link related issues, summarize schema or protocol changes, and attach screenshots/log excerpts when touching relay behaviour.
- PRs should outline manual verification steps (database migration, relay replay, etc.) so reviewers can reproduce quickly.

## Configuration & Security Notes
- Secrets live in `.env`; set `NOSTR_NSEC` to a valid NIP-19 encoded key before running the client.
- `data.sqlite3` is developer-local; do not commit generated data. Use `sql/backup.sql` to share deterministic seeds.
- Rotate relay URLs and reconnect intervals through `lib/config.ml`, and document notable changes in the PR body.
