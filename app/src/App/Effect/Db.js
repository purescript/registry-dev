import Database from 'better-sqlite3';


export const connect = () => {
  let db = new Database('db/registry.sqlite3', { fileMustExist: true });
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
  console.log(row);
  return row
};
