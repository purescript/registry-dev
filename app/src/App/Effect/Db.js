import Database from 'better-sqlite3';


export const connect = () => {
  let db = new Database('db/registry.sqlite3', { fileMustExist: true, verbose: console.log });
  db.pragma('journal_mode = WAL');
  return db;
}

export const insertLogImpl = (db) => (logLine) => () => {
  db
    .prepare('INSERT INTO logs (jobId, level, message, timestamp) VALUES (@jobId, @level, @message, @timestamp)')
    .run(logLine);
};

export const selectLogsByJobImpl = (db) => (jobId) => (logLevel) => () => {
  const row = db
    .prepare('SELECT * FROM logs WHERE jobId = ? AND level >= ? ORDER BY timestamp ASC')
    .all(jobId, logLevel);
  return row
};

export const createJobImpl = (db) => (job) => () => {
  db
    .prepare('INSERT INTO jobs (jobId, jobType, createdAt) VALUES (@jobId, @jobType, @createdAt)')
    .run(job);
};

export const finishJobImpl = (db) => (result) => () => {
  db
    .prepare('UPDATE jobs SET success = @success, finishedAt = @finishedAt WHERE jobId = @jobId')
    .run(result);
}

export const selectJobImpl = (db) => (jobId) => () => {
  const row = db
    .prepare('SELECT * FROM jobs WHERE jobId = ? LIMIT 1')
    .get(jobId);
  return row
}
