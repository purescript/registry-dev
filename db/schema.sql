CREATE TABLE IF NOT EXISTS "schema_migrations" (version varchar(128) primary key);
CREATE TABLE jobs (
  jobId text primary key not null,
  jobType text not null,
  createdAt text not null,
  finishedAt text,
  success integer not null default 0
);
CREATE TABLE logs (
  id integer primary key autoincrement,
  jobId text not null references jobs,
  level integer not null,
  message text not null,
  timestamp text not null
);
-- Dbmate schema migrations
INSERT INTO "schema_migrations" (version) VALUES
  ('20230711143615'),
  ('20230711143803');
