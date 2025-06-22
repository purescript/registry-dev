import Database from "better-sqlite3";

const JOB_INFO_TABLE = 'job_info'
const LOGS_TABLE = 'logs'
const PACKAGE_JOBS_TABLE = 'package_jobs';
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
    VALUES (@jobId, @createdAt, @startedAt, @finishedAt, @success
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

export const insertPackageJobImpl = (db, job) => {
  const columns = [ 'jobId', 'jobType', 'payload' ]
  return _insertJob(db, PACKAGE_JOBS_TABLE, columns, job);
};

export const insertMatrixJobImpl = (db, job) => {
  const columns = [ 'jobId', 'compilerVersion', 'payload' ]
  return _insertJob(db, MATRIX_JOBS_TABLE, columns, job);
};

export const insertPackageSetJobImpl = (db, job) => {
  const columns = [ 'jobId', 'payload' ]
  return _insertJob(db, PACKAGE_SET_JOBS_TABLE, columns, job);
};

export const selectNextPackageJobImpl = (db) => {
  const stmt = db.prepare(`
    SELECT job.*, info.createdAt, info.startedAt
    FROM ${PACKAGE_JOBS_TABLE} job
    JOIN ${JOB_INFO_TABLE} info ON job.jobId = info.jobId
    WHERE info.finishedAt IS NULL
    ORDER BY info.createdAt DESC
    LIMIT 1
  `);
  return stmt.get();
};

export const selectNextMatrixJobImpl = (db) => {
  const stmt = db.prepare(`
    SELECT job.*, info.createdAt, info.startedAt
    FROM ${MATRIX_JOBS_TABLE} job
    JOIN ${JOB_INFO_TABLE} info ON job.jobId = info.jobId
    WHERE info.finishedAt IS NULL
    ORDER BY info.createdAt DESC
    LIMIT 1
  `);
  return stmt.get();
};

export const selectNextPackageSetJobImpl = (db) => {
  const stmt = db.prepare(`
    SELECT job.*, info.createdAt, info.startedAt
    FROM ${PACKAGE_SET_JOBS_TABLE} job
    JOIN ${JOB_INFO_TABLE} info ON job.jobId = info.jobId
    WHERE info.finishedAt IS NULL
    ORDER BY info.createdAt DESC
    LIMIT 1
  `);
  return stmt.get();
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

export const deleteIncompleteJobsImpl = (db) => {
  const stmt = db.prepare(`DELETE FROM ${JOB_INFO_TABLE} WHERE finishedAt IS NULL`);
  return stmt.run();
};

export const insertLogLineImpl = (db, logLine) => {
  const stmt = db.prepare(`
    INSERT INTO ${LOGS_TABLE} (jobId, level, message, timestamp)
    VALUES (@jobId, @level, @message, @timestamp)
  `);
  return stmt.run(logLine);
};

export const selectLogsByJobImpl = (db, jobId, logLevel, since) => {
  let query = `
    SELECT * FROM ${LOGS_TABLE}
    WHERE jobId = ? AND level >= ?
  `;

  const params = [jobId, logLevel];

  if (since !== null) {
    query += ' AND timestamp >= ?';
    params.push(since);
  }

  query += ' ORDER BY timestamp ASC';

  const stmt = db.prepare(query);
  return stmt.all(...params);
};
