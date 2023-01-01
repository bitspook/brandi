-- id: 20230101113037
-- direction: UP
-- description: github_activity
PRAGMA foreign_keys = ON;
--;;
CREATE TABLE IF NOT EXISTS github_events (
  id TEXT PRIMARY KEY NOT NULL ON CONFLICT REPLACE,
  type TEXT NOT NULL,
  payload TEXT NOT NULL,
  created_at TEXT NOT NULL default current_timestamp,
  updated_at TEXT NOT NULL default current_timestamp
) STRICT, WITHOUT ROWID;
--;;
