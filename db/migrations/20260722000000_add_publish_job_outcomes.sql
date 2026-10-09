-- migrate:up

-- Successful publish jobs expose whether this attempt completed publication or
-- found an equivalent version already published. Failed jobs expose their
-- terminal error without requiring clients to parse logs.
ALTER TABLE job_info ADD COLUMN disposition TEXT;
ALTER TABLE job_info ADD COLUMN errorCode TEXT;
ALTER TABLE job_info ADD COLUMN errorMessage TEXT;

-- migrate:down

ALTER TABLE job_info DROP COLUMN errorMessage;
ALTER TABLE job_info DROP COLUMN errorCode;
ALTER TABLE job_info DROP COLUMN disposition;
