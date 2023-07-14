import Database from "better-sqlite3";

export const connect = (path) => () => {
  console.log("Connecting to database at " + path);
  let db = new Database(path, {
    fileMustExist: true,
    verbose: console.log,
  });
  db.pragma("journal_mode = WAL");
  db.pragma("foreign_keys = ON");
  return db;
};

export const insertLogImpl = (db) => (logLine) => () => {
  db
    .prepare("INSERT INTO logs (jobId, level, message, timestamp) VALUES (@jobId, @level, @message, @timestamp)")
    .run(logLine);
};

export const selectLogsByJobImpl = (db) => (jobId) => (logLevel) => () => {
  const row = db
    .prepare("SELECT * FROM logs WHERE jobId = ? AND level >= ? ORDER BY timestamp ASC")
    .all(jobId, logLevel);
  return row;
};

export const createJobImpl = (db) => (job) => () => {
  db
    .prepare("INSERT INTO jobs (jobId, jobType, createdAt, packageName, ref) VALUES (@jobId, @jobType, @createdAt, @packageName, @ref)")
    .run(job);
};

export const finishJobImpl = (db) => (result) => () => {
  db
    .prepare("UPDATE jobs SET success = @success, finishedAt = @finishedAt WHERE jobId = @jobId")
    .run(result);
};

export const selectJobImpl = (db) => (jobId) => () => {
  const row = db
    .prepare("SELECT * FROM jobs WHERE jobId = ? LIMIT 1")
    .get(jobId);
  return row;
}

export const runningJobForPackageImpl = (db) => (packageName) => () => {
  const row = db
    .prepare("SELECT * FROM jobs WHERE finishedAt IS NULL AND packageName = ? ORDER BY createdAt ASC LIMIT 1")
    .get(packageName);
  return row;
}

export const deleteIncompleteJobsImpl = (db) => () => {
  db
    .prepare("DELETE FROM jobs WHERE finishedAt IS NULL")
    .run();
}
