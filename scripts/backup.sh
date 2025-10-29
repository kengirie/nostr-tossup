#!/usr/bin/env bash
set -euo pipefail

# Nostr Tossup Database Backup Script
SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PROJECT_DIR="$(dirname "$SCRIPT_DIR")"
BACKUP_DIR="$PROJECT_DIR/backups"
DB_PATH="$PROJECT_DIR/data/data.sqlite3"
TIMESTAMP=$(date +"%Y%m%d_%H%M%S")
BACKUP_FILE="$BACKUP_DIR/backup_$TIMESTAMP.db"

# Create backup directory if it doesn't exist
mkdir -p "$BACKUP_DIR"

# Create backup
if [ -f "$DB_PATH" ]; then
    sqlite3 "$DB_PATH" ".backup '$BACKUP_FILE'"
    echo "$(date): Backup created: $BACKUP_FILE"
    
    # Cleanup old backups (keep only last 7 days)
    find "$BACKUP_DIR" -name "backup_*.db" -mtime +7 -delete
    echo "$(date): Cleaned up backups older than 7 days"
else
    echo "$(date): Database file not found: $DB_PATH"
fi