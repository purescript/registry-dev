-- migrate:up

-- Poll a job's time window in timestamp order without scanning other jobs or
-- sorting. Keep level as a residual filter so both sort directions use this index.
CREATE INDEX idx_logs_job_timestamp ON logs (jobId, timestamp);

-- migrate:down

DROP INDEX idx_logs_job_timestamp;
