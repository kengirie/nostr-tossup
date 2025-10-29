#!/usr/bin/env bash
set -euo pipefail

# Setup cron job for hourly backups
SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
DEFAULT_BACKUP_RELATIVE="backup.sh"
BACKUP_SCRIPT_INPUT="${BACKUP_SCRIPT_PATH:-$DEFAULT_BACKUP_RELATIVE}"

if [[ "$BACKUP_SCRIPT_INPUT" = /* ]]; then
  BACKUP_SCRIPT_ABS="$BACKUP_SCRIPT_INPUT"
  case "$BACKUP_SCRIPT_INPUT" in
    "$SCRIPT_DIR"/*)
      BACKUP_SCRIPT_REL="${BACKUP_SCRIPT_INPUT#"$SCRIPT_DIR"/}"
      ;;
    *)
      BACKUP_SCRIPT_REL=""
      ;;
  esac
else
  BACKUP_SCRIPT_REL="$BACKUP_SCRIPT_INPUT"
  BACKUP_SCRIPT_ABS="$(cd "$SCRIPT_DIR" && cd "$(dirname "$BACKUP_SCRIPT_REL")" && pwd)/$(basename "$BACKUP_SCRIPT_REL")"
fi

echo "Setting up hourly backup cron job..."
[[ -n "$BACKUP_SCRIPT_REL" ]] && echo "Relative path: $BACKUP_SCRIPT_REL"
echo "Absolute path: $BACKUP_SCRIPT_ABS"

# Make backup script executable
chmod +x "$BACKUP_SCRIPT_ABS"

# Add cron job (every hour at minute 0)
(crontab -l 2>/dev/null || echo "") \
  | grep -vF "$BACKUP_SCRIPT_ABS" \
  | (cat; echo "0 * * * * $BACKUP_SCRIPT_ABS >> /var/log/backup.log 2>&1") \
  | crontab -

echo "Cron job added: hourly backup at minute 0"
echo "Logs will be written to /var/log/backup.log"

# Show current crontab
echo "Current crontab:"
crontab -l
