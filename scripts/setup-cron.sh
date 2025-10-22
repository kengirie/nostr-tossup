#!/bin/bash

# Setup cron job for hourly backups
BACKUP_SCRIPT_PATH="/app/scripts/backup.sh"

echo "Setting up hourly backup cron job..."

# Make backup script executable
chmod +x "$BACKUP_SCRIPT_PATH"

# Add cron job (every hour at minute 0)
(crontab -l 2>/dev/null || echo "") | grep -v "$BACKUP_SCRIPT_PATH" | (cat; echo "0 * * * * $BACKUP_SCRIPT_PATH >> /var/log/backup.log 2>&1") | crontab -

echo "Cron job added: hourly backup at minute 0"
echo "Logs will be written to /var/log/backup.log"

# Show current crontab
echo "Current crontab:"
crontab -l