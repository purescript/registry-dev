CREATE TABLE IF NOT EXISTS "schema_migrations" (version varchar(128) primary key);
CREATE TABLE job_info (
  jobId TEXT PRIMARY KEY NOT NULL,
  createdAt TEXT NOT NULL,
  startedAt TEXT,
  finishedAt TEXT,
  success INTEGER NOT NULL DEFAULT 0
);
CREATE TABLE package_jobs (
  jobId TEXT PRIMARY KEY NOT NULL,
  jobType TEXT NOT NULL,
  packageName TEXT NOT NULL,
  payload JSON NOT NULL,
  FOREIGN KEY (jobId) REFERENCES job_info (jobId) ON DELETE CASCADE
);
CREATE TABLE matrix_jobs (
  jobId TEXT PRIMARY KEY NOT NULL,
  packageName TEXT NOT NULL,
  packageVersion TEXT NOT NULL,
  compilerVersion TEXT NOT NULL,
  -- the build plan, which should be computed before the job is stored in the
  -- queue so that if multiple jobs targeting one package get interrupted by
  -- a higher-priority job then the build plan is not affected.
  payload JSON NOT NULL,
  FOREIGN KEY (jobId) REFERENCES job_info (jobId) ON DELETE CASCADE
);
CREATE TABLE package_set_jobs (
  jobId TEXT PRIMARY KEY NOT NULL,
  payload JSON NOT NULL,
  FOREIGN KEY (jobId) REFERENCES job_info (jobId) ON DELETE CASCADE
);
CREATE TABLE logs (
  id INTEGER PRIMARY KEY AUTOINCREMENT,
  jobId TEXT NOT NULL REFERENCES job_info (jobId) ON DELETE CASCADE,
  level INTEGER NOT NULL,
  message TEXT NOT NULL,
  timestamp TEXT NOT NULL
);
-- Dbmate schema migrations
INSERT INTO "schema_migrations" (version) VALUES
  ('20230711143615'),
  ('20230711143803'),
  ('20240914170550'),
  ('20240914171030');
