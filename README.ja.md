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

> `Package nostr-tossup does not exist, create as a NEW package? [Y/n]` と表示された場合は `Y`（Enter）でローカルパッケージとして登録してください。

### 3. 自動バックアップの設定
```bash
# バックアップスクリプトに実行権限付与
chmod +x scripts/*.sh

# cron設定（1時間毎にバックアップ、7日間保持）
./scripts/setup-cron.sh
```

### 4. `.env` に秘密鍵を保存
```bash
cat <<'EOF' > .env
NOSTR_NSEC=nsec1your_secret_key_here
EOF
```

起動時に `.env` を自動読み込みするため、上記の値を自分の NIP-19 秘密鍵に書き換えてください。

### 5. 実行
```bash
# フォアグラウンド実行
./_build/default/bin/main.exe

# バックグラウンド実行
nohup ./_build/default/bin/main.exe > app.log 2>&1 &
```

> Azure などの最小構成ディストリビューションでは、以下のように `libsecp256k1` を事前にロードする必要がある場合があります。
> ```bash
> LD_PRELOAD=/lib/x86_64-linux-gnu/libsecp256k1.so dune exec bin/main.exe
> ```

## 本番運用

### プロセス管理（screen活用例）
```bash
# nostr という screen セッションを作成（既にあれば再接続）
screen -S nostr

# screen セッション内でアプリを起動（.env を自動読み込み）
dune exec bin/main.exe

# アプリを止めずにセッションを離れる
# Ctrl+a → d

# 後で戻るとき
screen -r nostr

# 停止したい場合は再接続して Ctrl+C
```

### アップデート手順
```bash
cd nostr-tossup
screen -r nostr   # 起動中ならセッションへ戻る
# セッション内でアプリを Ctrl+C で停止
git pull origin main
dune build
dune exec bin/main.exe  # screen 内で再起動
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
