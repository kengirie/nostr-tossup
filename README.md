# nostr-tossup

OCaml client that subscribes to Nostr relays for kind:1 events, filters Japanese newcomers whose first post is within the last 30 days, and republishes them as kind:30078 events. It uses the Eio runtime together with the Piaf WebSocket client.

## Requirements

- OCaml 5.3+
- opam 2.3+
- Ubuntu 24.04 LTS or similar (for the setup instructions below)

See `README.ja.md` for the Japanese version of these instructions.

## Ubuntu Setup

### 1. Install system dependencies
```bash
sudo apt update
sudo apt install -y \
  opam sqlite3 cron \
  build-essential \
  libffi-dev \
  libgmp-dev \
  libsqlite3-dev \
  libssl-dev \
  libsecp256k1-dev \
  pkg-config \
  git
```

### 2. Clone and build the project
```bash
git clone https://github.com/kengirie/nostr-tossup
cd nostr-tossup

# Initialize opam (first time only)
opam init -y --disable-sandboxing
eval $(opam env)

# Create a project-local switch on OCaml 5.3.0
opam switch create . 5.3.0
eval $(opam env)

# Install dependencies and build
opam install . --deps-only -y
dune build
```

> If opam prints `Package nostr-tossup does not exist, create as a NEW package? [Y/n]`, answer `Y` (or press Enter) to register the local package.

### 3. Configure automated backups
```bash
chmod +x scripts/*.sh
./scripts/setup-cron.sh  # hourly backups, 7-day retention
```

### 4. Create the `.env` secret file
```bash
cat <<'EOF' > .env
NOSTR_NSEC=nsec1your_secret_key_here
EOF
```

The app loads `.env` automatically at startup. Adjust the value above to your own NIP-19 secret.

### 5. Run the application
```bash
./_build/default/bin/main.exe                         # foreground
nohup ./_build/default/bin/main.exe > app.log 2>&1 &  # background
```

> On some distributions (e.g., minimal Azure images) you may need to preload `libsecp256k1`:
> ```bash
> LD_PRELOAD=/lib/x86_64-linux-gnu/libsecp256k1.so dune exec bin/main.exe
> ```

## Production Notes

### screen session example
```bash
# Create or reattach to a screen session named "nostr"
screen -S nostr

# Inside screen, start the app (loads .env by default)
dune exec bin/main.exe

# Detach without stopping the app
# (press Ctrl+a, then d)

# Reattach later
screen -r nostr

# To stop the app, reattach and press Ctrl+C inside screen
```

### Updating
```bash
cd nostr-tossup
screen -r nostr   # reattach if already running
# Stop the running process inside screen (Ctrl+C)
git pull origin main
dune build
dune exec bin/main.exe  # restart inside screen
```

## Features

- **Relay subscriptions** — listen to multiple relays for kind:1 events.
- **Japanese content detection** — pick up hiragana/katakana/kanji posts from newcomers.
- **SQLite persistence** — store user metadata and publication history locally.
- **Scheduled publishing** — send kind:30078 contact lists and summaries on an interval.
- **Automated backups** — optional cron job for hourly database snapshots (7-day retention).

## Configuration Touchpoints

- `lib/config.ml` → `subscribe_relays`, `publish_relays`, `reconnect_delay`
- `sql/schema.sql`, `sql/backup.sql` → keep schema changes in sync
- `.env` → set `NOSTR_NSEC` before running

## Local Development

1. `opam install . --deps-only`
2. `eval $(opam env)`
3. `dune build`
4. `export NOSTR_NSEC="nsec1your_test_key"`
5. `dune exec bin/main.exe`

Run `dune test` to execute the unit suite. Keep `sql/schema.sql` and `sql/backup.sql` aligned whenever the schema changes.
