-- migrate:up

-- Common job information table
CREATE TABLE job_info (
  jobId TEXT PRIMARY KEY NOT NULL,
  createdAt TEXT NOT NULL,
  startedAt TEXT,
  finishedAt TEXT,
  success INTEGER NOT NULL DEFAULT 0
);

-- Package-oriented jobs (publish/unpublish/transfer)
CREATE TABLE package_jobs (
  jobId TEXT PRIMARY KEY NOT NULL,
  jobType TEXT NOT NULL,
  packageName TEXT NOT NULL,
  packageVersion TEXT NOT NULL,
  payload JSON NOT NULL,
  FOREIGN KEY (jobId) REFERENCES job_info (jobId) ON DELETE CASCADE
);

-- Compiler matrix jobs (one compiler, all packages)
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

-- Package set jobs
CREATE TABLE package_set_jobs (
  jobId TEXT PRIMARY KEY NOT NULL,
  payload JSON NOT NULL,
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
DROP TABLE package_jobs;
DROP TABLE matrix_jobs;
DROP TABLE package_set_jobs;
DROP TABLE logs;
