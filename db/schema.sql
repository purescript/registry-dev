CREATE TABLE IF NOT EXISTS "schema_migrations" (version varchar(128) primary key);

-- FIXME: Remember the impl needs to be in the migrations files, not here, as
-- this file is generated. See if a table can be renamed? (ALTER TABLE)

-- FIXME: This should be a 'package jobs' table
CREATE TABLE jobs (
  jobId text primary key not null,
  jobType text not null,
  packageName text,
  ref text not null,
  createdAt text not null,

  -- FIXME: startedAt text, -- indicates a job has been picked up

  finishedAt text,
  success integer not null default 0
);

-- FIXME: New tables for new job types, ie. a 'package sets' table and a
-- 'compiler matrix' table.

-- FIXME: How to unify logs across job types? One logs table per job type table
CREATE TABLE logs (
  id integer primary key autoincrement,
  jobId text not null references jobs on delete cascade,
  level integer not null,
  message text not null,
  timestamp text not null
);

-- Dbmate schema migrations
INSERT INTO "schema_migrations" (version) VALUES
  ('20230711143615'),
  ('20230711143803');
