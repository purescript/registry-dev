-- migrate:up

-- Common job information table
CREATE TABLE job_info (
  jobId TEXT PRIMARY KEY NOT NULL,
  createdAt TEXT NOT NULL,
  startedAt TEXT,
  finishedAt TEXT,
  success INTEGER NOT NULL DEFAULT 0
);

-- Publishing jobs
CREATE TABLE publish_jobs (
  jobId TEXT PRIMARY KEY NOT NULL,
  packageName TEXT NOT NULL,
  packageVersion TEXT NOT NULL,
  payload JSON NOT NULL,
  FOREIGN KEY (jobId) REFERENCES job_info (jobId) ON DELETE CASCADE
);

-- Unpublishing jobs
CREATE TABLE unpublish_jobs (
  jobId TEXT PRIMARY KEY NOT NULL,
  packageName TEXT NOT NULL,
  packageVersion TEXT NOT NULL,
  payload JSON NOT NULL,
  FOREIGN KEY (jobId) REFERENCES job_info (jobId) ON DELETE CASCADE
);

-- Package transfer jobs
CREATE TABLE transfer_jobs (
  jobId TEXT PRIMARY KEY NOT NULL,
  packageName TEXT NOT NULL,
  payload JSON NOT NULL,
  FOREIGN KEY (jobId) REFERENCES job_info (jobId) ON DELETE CASCADE
);

-- Compiler matrix jobs
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

-- Admin jobs (scheduled tasks and manual package set operations)
CREATE TABLE admin_jobs (
  jobId TEXT PRIMARY KEY NOT NULL,
  adminJobType TEXT NOT NULL,  -- 'package_transfer', 'legacy_import', 'package_set_update', 'package_set_operation'
  payload JSON NOT NULL,
  -- Keep these for manual package set operations (authenticated API requests)
  rawPayload TEXT,
  signature TEXT,
  FOREIGN KEY (jobId) REFERENCES job_info (jobId) ON DELETE CASCADE
);

CREATE TABLE IF NOT EXISTS logs (
  id INTEGER PRIMARY KEY AUTOINCREMENT,
  jobId TEXT NOT NULL REFERENCES job_info (jobId) ON DELETE CASCADE,
  level INTEGER NOT NULL,
  message TEXT NOT NULL,
  timestamp TEXT NOT NULL
);

-- migrate:down

DROP TABLE job_info;
DROP TABLE publish_jobs;
DROP TABLE unpublish_jobs;
DROP TABLE transfer_jobs;
DROP TABLE matrix_jobs;
DROP TABLE admin_jobs;
DROP TABLE logs;
