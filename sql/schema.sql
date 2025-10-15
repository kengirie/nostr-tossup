-- Create users table
CREATE TABLE users (
  pubkey TEXT UNIQUE NOT NULL PRIMARY KEY,
  registration_date DATE NOT NULL,
  existing_user INTEGER DEFAULT 1
);
