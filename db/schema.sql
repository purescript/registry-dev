CREATE TABLE IF NOT EXISTS "schema_migrations" (version varchar(128) primary key);
CREATE TABLE logs (
  id integer primary key autoincrement,
  jobId text not null,
  level integer not null,
  message text not null,
  timestamp text not null
);
-- Dbmate schema migrations
INSERT INTO "schema_migrations" (version) VALUES
  ('20230708113837');
