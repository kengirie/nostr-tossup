# nostr-tossup

OCaml client that subscribes to Nostr relays for kind:1 events, filters Japanese newcomers whose first post is within the last 30 days, and republishes them as kind:30078 events. It uses the Eio runtime together with the Piaf WebSocket client.

## Requirements

- OCaml 5.3+
- opam 2.3+
- Ubuntu 24.04 LTS or similar (for the setup instructions below)

See `README.ja.md` for the Japanese version of these instructions.

## Ubuntu VM Setup

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

### 3. Configure automated backups
```bash
chmod +x scripts/*.sh
./scripts/setup-cron.sh  # hourly backups, 7-day retention
```

### 4. Set environment variables
```bash
export NOSTR_NSEC="nsec1your_secret_key_here"
echo 'export NOSTR_NSEC="nsec1your_secret_key_here"' >> ~/.bashrc  # optional persistence
```

### 5. Run the application
```bash
./_build/default/bin/main.exe                        # foreground
nohup ./_build/default/bin/main.exe > app.log 2>&1 &  # background
```

## Production Notes

### systemd service example
```bash
sudo tee /etc/systemd/system/nostr-tossup.service <<'EOF'
[Unit]
Description=Nostr Tossup Service
After=network.target

[Service]
Type=simple
User=ubuntu
WorkingDirectory=/home/ubuntu/nostr-tossup
Environment=NOSTR_NSEC=nsec1your_secret_key_here
ExecStart=/home/ubuntu/nostr-tossup/_build/default/bin/main.exe
Restart=always
RestartSec=10

[Install]
WantedBy=multi-user.target
EOF

sudo systemctl enable nostr-tossup
sudo systemctl start nostr-tossup
sudo journalctl -u nostr-tossup -f
```

### Updating
```bash
cd nostr-tossup
git pull origin main
dune build
sudo systemctl restart nostr-tossup
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
