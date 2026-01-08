CREATE TABLE IF NOT EXISTS "schema_migrations" (version varchar(128) primary key);
CREATE TABLE job_info (
  jobId TEXT PRIMARY KEY NOT NULL,
  createdAt TEXT NOT NULL,
  startedAt TEXT,
  finishedAt TEXT,
  success INTEGER NOT NULL DEFAULT 0
);
CREATE TABLE publish_jobs (
  jobId TEXT PRIMARY KEY NOT NULL,
  packageName TEXT NOT NULL,
  packageVersion TEXT NOT NULL,
  payload JSON NOT NULL,
  FOREIGN KEY (jobId) REFERENCES job_info (jobId) ON DELETE CASCADE
);
CREATE TABLE unpublish_jobs (
  jobId TEXT PRIMARY KEY NOT NULL,
  packageName TEXT NOT NULL,
  packageVersion TEXT NOT NULL,
  payload JSON NOT NULL,
  FOREIGN KEY (jobId) REFERENCES job_info (jobId) ON DELETE CASCADE
);
CREATE TABLE transfer_jobs (
  jobId TEXT PRIMARY KEY NOT NULL,
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
CREATE TABLE admin_jobs (
  jobId TEXT PRIMARY KEY NOT NULL,
  adminJobType TEXT NOT NULL,  -- 'package_transfer', 'legacy_import', 'package_set_update', 'package_set_operation'
  payload JSON NOT NULL,
  -- Keep these for manual package set operations (authenticated API requests)
  rawPayload TEXT,
  signature TEXT,
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
