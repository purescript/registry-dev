-- migrate:up

-- For duplicate detection in publish jobs
CREATE INDEX IF NOT EXISTS idx_publish_jobs_name_version
ON publish_jobs (packageName, packageVersion);

-- For duplicate detection in unpublish jobs
CREATE INDEX IF NOT EXISTS idx_unpublish_jobs_name_version
ON unpublish_jobs (packageName, packageVersion);

-- For duplicate detection in transfer jobs
CREATE INDEX IF NOT EXISTS idx_transfer_jobs_name
ON transfer_jobs (packageName);

-- For job selection performance
CREATE INDEX IF NOT EXISTS idx_job_info_pending
ON job_info (createdAt)
WHERE finishedAt IS NULL AND startedAt IS NULL;

-- migrate:down

DROP INDEX IF EXISTS idx_publish_jobs_name_version;
DROP INDEX IF EXISTS idx_unpublish_jobs_name_version;
DROP INDEX IF EXISTS idx_transfer_jobs_name;
DROP INDEX IF EXISTS idx_job_info_pending;
