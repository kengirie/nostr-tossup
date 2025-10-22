# nostr-tossup

Nostr リレーから kind:1 イベントを購読し、日本人ユーザーで初めての kind:1 投稿が 30 日以内の npub を抽出して kind:30078 イベントとして定期配信する OCaml製クライアントです。Eio ランタイムと WebSocket クライアントライブラリの Piaf を併用しています。

## 環境要件

- OCaml 5.3+
- opam 2.3+
- Ubuntu 24.04 LTS (推奨)

## Ubuntu VMでのセットアップ

### 1. システム依存関係のインストール
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

### 2. プロジェクトのクローンとビルド
```bash
# プロジェクトクローン
git clone https://github.com/kengirie/nostr-tossup
cd nostr-tossup

# opam初期化（初回のみ）
opam init -y --disable-sandboxing
eval $(opam env)

# プロジェクト専用スイッチを作成 (OCaml 5.3.0)
opam switch create . 5.3.0
eval $(opam env)

# 依存関係インストール
opam install . --deps-only -y

# ビルド
dune build
```

### 3. 自動バックアップの設定
```bash
# バックアップスクリプトに実行権限付与
chmod +x scripts/*.sh

# cron設定（1時間毎にバックアップ、7日間保持）
./scripts/setup-cron.sh
```

### 4. 環境変数設定
```bash
# NOSTR_NSEC秘密鍵を環境変数に設定
export NOSTR_NSEC="nsec1your_secret_key_here"

# 永続化する場合は ~/.bashrc に追加
echo 'export NOSTR_NSEC="nsec1your_secret_key_here"' >> ~/.bashrc
```

### 5. 実行
```bash
# フォアグラウンド実行
./_build/default/bin/main.exe

# バックグラウンド実行
nohup ./_build/default/bin/main.exe > app.log 2>&1 &
```

## 本番運用

### プロセス管理（systemd推奨）
```bash
# systemdサービス作成例
sudo tee /etc/systemd/system/nostr-tossup.service << EOF
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

# サービス有効化・開始
sudo systemctl enable nostr-tossup
sudo systemctl start nostr-tossup

# ログ確認
sudo journalctl -u nostr-tossup -f
```

### アップデート手順
```bash
cd nostr-tossup
git pull origin main
dune build
sudo systemctl restart nostr-tossup
```

## 機能

- **Nostrリレー接続**: 複数リレーからkind1イベントを購読
- **日本語コンテンツ検出**: ひらがな・カタカナを含むイベントを自動検出
- **ユーザー管理**: SQLiteデータベースでユーザー情報を管理
- **定期配信**: kind30078イベントとしてリストを定期更新
- **自動バックアップ**: 1時間毎のデータベースバックアップ（7日間保持）

## 設定

- **購読リレー**: `lib/config.ml` の `subscribe_relays`
- **配信リレー**: `lib/config.ml` の `publish_relays`
- **再接続設定**: `lib/config.ml` の `reconnect_delay`
- **データベース**: `data.sqlite3`（自動作成）

## ローカル開発

1. `opam install . --deps-only`
2. `eval $(opam env)`
3. `dune build`
4. `export NOSTR_NSEC="nsec1your_test_key"`
5. `dune exec bin/main.exe`
