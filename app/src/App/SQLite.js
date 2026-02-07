import Database from "better-sqlite3";

const JOB_INFO_TABLE = 'job_info'
const LOGS_TABLE = 'logs'
const PUBLISH_JOBS_TABLE = 'publish_jobs';
const UNPUBLISH_JOBS_TABLE = 'unpublish_jobs';
const TRANSFER_JOBS_TABLE = 'transfer_jobs';
const MATRIX_JOBS_TABLE = 'matrix_jobs';
const PACKAGE_SET_JOBS_TABLE = 'package_set_jobs';

export const connectImpl = (path, logger) => {
  logger("Connecting to database at " + path);
  let db = new Database(path, {
    fileMustExist: true,
    verbose: logger,
  });
  db.pragma("journal_mode = WAL");
  db.pragma("foreign_keys = ON");
  return db;
};

export const selectJobInfoImpl = (db, jobId) => {
  const stmt = db.prepare(`
    SELECT * FROM ${JOB_INFO_TABLE}
    WHERE jobId = ? LIMIT 1
  `);
  return stmt.get(jobId);
}

// A generic helper function for inserting a new package, matrix, or package set
// job Not exported because this should always be done as part of a more general
// job insertion. A job is expected to always include a 'jobId' and 'createdAt'
// field, though other fields will be required depending on the job.
const _insertJob = (db, table, columns, job) => {
  const requiredFields = Array.from(new Set(['jobId', 'createdAt', ...columns]));
  const missingFields = requiredFields.filter(field => !(field in job));
  const extraFields = Object.keys(job).filter(field => !requiredFields.includes(field));

  if (missingFields.length > 0) {
    throw new Error(`Missing required fields for insertion: ${missingFields.join(', ')}`);
  }

  if (extraFields.length > 0) {
    throw new Error(`Unexpected extra fields for insertion: ${extraFields.join(', ')}`);
  }

  const insertInfo = db.prepare(`
    INSERT INTO ${JOB_INFO_TABLE} (jobId, createdAt, startedAt, finishedAt, success)
    VALUES (@jobId, @createdAt, @startedAt, @finishedAt, @success)
  `);

  const insertJob = db.prepare(`
    INSERT INTO ${table} (${columns.join(', ')})
    VALUES (${columns.map(col => `@${col}`).join(', ')})
  `);

  const insert = db.transaction((job) => {
    insertInfo.run({
      jobId: job.jobId,
      createdAt: job.createdAt,
      startedAt: null,
      finishedAt: null,
      success: 0
    });
    insertJob.run(job);
  });

  return insert(job);
};

export const insertPublishJobImpl = (db, job) => {
  const columns = ['jobId', 'packageName', 'packageVersion', 'payload']
  return _insertJob(db, PUBLISH_JOBS_TABLE, columns, job);
};

export const insertUnpublishJobImpl = (db, job) => {
  const columns = ['jobId', 'packageName', 'packageVersion', 'payload']
  return _insertJob(db, UNPUBLISH_JOBS_TABLE, columns, job);
};

export const insertTransferJobImpl = (db, job) => {
  const columns = ['jobId', 'packageName', 'payload']
  return _insertJob(db, TRANSFER_JOBS_TABLE, columns, job);
};

export const insertMatrixJobImpl = (db, job) => {
  const columns = ['jobId', 'packageName', 'packageVersion', 'compilerVersion', 'payload'];

  // Check if a matrix job already exists for this package/version/compiler
  // (either pending or completed - we don't want to re-run completed jobs either)
  const existingJob = db.prepare(`
    SELECT job.jobId FROM ${MATRIX_JOBS_TABLE} job
    WHERE job.packageName = ? AND job.packageVersion = ? AND job.compilerVersion = ?
    LIMIT 1
  `).get(job.packageName, job.packageVersion, job.compilerVersion);

  if (existingJob) {
    // Return the existing job's ID instead of inserting a duplicate
    return existingJob.jobId;
  }

  _insertJob(db, MATRIX_JOBS_TABLE, columns, job);
  return job.jobId;
};

export const insertPackageSetJobImpl = (db, job) => {
  const columns = ['jobId', 'payload', 'rawPayload', 'signature']
  return _insertJob(db, PACKAGE_SET_JOBS_TABLE, columns, job);
};

const _selectJob = (db, { table, jobId, packageName, packageVersion }) => {
  const params = [];
  let query = `
    SELECT job.*, info.*
    FROM ${table} job
    JOIN ${JOB_INFO_TABLE} info ON job.jobId = info.jobId
  `;

  if (jobId != null) {
    query += ` WHERE info.jobId = ?`;
    params.push(jobId);
  } else if (packageName != null) {
    query += ` WHERE job.packageName = ?`;
    params.push(packageName);
    if (packageVersion != null) {
      query += ` AND job.packageVersion = ?`;
      params.push(packageVersion);
    }
  } else {
    query += ` WHERE info.finishedAt IS NULL AND info.startedAt IS NULL`;
  }

  query += ` ORDER BY info.createdAt ASC LIMIT 1`;
  const stmt = db.prepare(query);

  return stmt.get(...params);
}

export const selectPublishJobImpl = (db, { jobId, packageName, packageVersion }) => {
  return _selectJob(db, { table: PUBLISH_JOBS_TABLE, jobId, packageName, packageVersion });
};

export const selectUnpublishJobImpl = (db, { jobId, packageName, packageVersion }) => {
  return _selectJob(db, { table: UNPUBLISH_JOBS_TABLE, jobId, packageName, packageVersion });
};

export const selectTransferJobImpl = (db, { jobId, packageName }) => {
  return _selectJob(db, { table: TRANSFER_JOBS_TABLE, jobId, packageName });
};

export const selectMatrixJobImpl = (db, jobId) => {
  return _selectJob(db, { table: MATRIX_JOBS_TABLE, jobId });
};

export const selectPackageSetJobImpl = (db, jobId) => {
  return _selectJob(db, { table: PACKAGE_SET_JOBS_TABLE, jobId });
};

// Find a pending package set job by payload (for duplicate detection)
export const selectPackageSetJobByPayloadImpl = (db, payload) => {
  const stmt = db.prepare(`
    SELECT job.*, info.*
    FROM ${PACKAGE_SET_JOBS_TABLE} job
    JOIN ${JOB_INFO_TABLE} info ON job.jobId = info.jobId
    WHERE job.payload = ? AND info.finishedAt IS NULL
    ORDER BY info.createdAt ASC LIMIT 1
  `);
  return stmt.get(payload);
};

const _selectJobs = (db, { table, since, until, includeCompleted }) => {
  let query = `
    SELECT job.*, info.*
    FROM ${table} job
    JOIN ${JOB_INFO_TABLE} info ON job.jobId = info.jobId
    WHERE 1=1
  `;
  let params = [];

  if (since != null) {
    query += ` AND info.createdAt >= ?`;
    params.push(since);
  }

  if (until != null) {
    query += ` AND info.createdAt <= ?`;
    params.push(until);
  }

  if (includeCompleted === false) {
    query += ` AND info.finishedAt IS NULL`;
  }

  query += ` ORDER BY info.createdAt ASC LIMIT 100`;
  const stmt = db.prepare(query);

  return stmt.all(...params);
}

export const selectPublishJobsImpl = (db, since, until, includeCompleted) => {
  return _selectJobs(db, { table: PUBLISH_JOBS_TABLE, since, until, includeCompleted });
};

export const selectUnpublishJobsImpl = (db, since, until, includeCompleted) => {
  return _selectJobs(db, { table: UNPUBLISH_JOBS_TABLE, since, until, includeCompleted });
};

export const selectTransferJobsImpl = (db, since, until, includeCompleted) => {
  return _selectJobs(db, { table: TRANSFER_JOBS_TABLE, since, until, includeCompleted });
};

export const selectMatrixJobsImpl = (db, since, until, includeCompleted) => {
  return _selectJobs(db, { table: MATRIX_JOBS_TABLE, since, until, includeCompleted });
};

export const selectPackageSetJobsImpl = (db, since, until, includeCompleted) => {
  return _selectJobs(db, { table: PACKAGE_SET_JOBS_TABLE, since, until, includeCompleted });
};

export const startJobImpl = (db, args) => {
  const stmt = db.prepare(`
    UPDATE ${JOB_INFO_TABLE}
    SET startedAt = @startedAt
    WHERE jobId = @jobId
  `);
  return stmt.run(args);
}

export const finishJobImpl = (db, args) => {
  const stmt = db.prepare(`
    UPDATE ${JOB_INFO_TABLE}
    SET success = @success, finishedAt = @finishedAt
    WHERE jobId = @jobId
  `);
  return stmt.run(args);
}

// Reset incomplete jobs, incrementing their resetCount.
// Returns the list of job IDs that have been reset 3+ times (livelocked).
export const resetIncompleteJobsImpl = (db) => {
  const LIVELOCK_THRESHOLD = 3;

  // Reset incomplete jobs and increment their resetCount
  const updateStmt = db.prepare(`
    UPDATE ${JOB_INFO_TABLE}
    SET startedAt = NULL, resetCount = resetCount + 1
    WHERE finishedAt IS NULL
    AND startedAt IS NOT NULL
  `);
  updateStmt.run();

  // Return jobs that have hit the livelock threshold
  const selectStmt = db.prepare(`
    SELECT jobId FROM ${JOB_INFO_TABLE}
    WHERE finishedAt IS NULL
    AND resetCount >= ?
  `);
  const livelockedJobIds = selectStmt.all(LIVELOCK_THRESHOLD).map(row => row.jobId);

  return livelockedJobIds;
};

export const insertLogLineImpl = (db, logLine) => {
  const stmt = db.prepare(`
    INSERT INTO ${LOGS_TABLE} (jobId, level, message, timestamp)
    VALUES (@jobId, @level, @message, @timestamp)
  `);
  return stmt.run(logLine);
};

export const selectLogsByJobImpl = (db, jobId, logLevel, since, until) => {
  let query = `
    SELECT * FROM ${LOGS_TABLE}
    WHERE jobId = ? AND level >= ?
  `;
  let params = [jobId, logLevel];

  if (since != null) {
    query += ` AND timestamp >= ?`;
    params.push(since);
  }

  if (until != null) {
    query += ` AND timestamp <= ?`;
    params.push(until);
  }

  query += ` ORDER BY timestamp ASC LIMIT 100`;
  const stmt = db.prepare(query);
  return stmt.all(...params);
};
