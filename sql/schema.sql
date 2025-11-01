PRAGMA defer_foreign_keys=TRUE;
CREATE TABLE IF NOT EXISTS users (
  pubkey TEXT UNIQUE NOT NULL PRIMARY KEY,
  registration_date DATE NOT NULL,
  existing_user INTEGER DEFAULT 1,
  is_bot INTEGER NOT NULL DEFAULT 2
);
