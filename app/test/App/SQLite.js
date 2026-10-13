import Database from "better-sqlite3";
import { readFileSync } from "node:fs";
import { randomUUID } from "node:crypto";

export const withDatabase = (test) => () => {
  const db = new Database(":memory:");
  try {
    db.pragma("foreign_keys = ON");
    db.exec(readFileSync("db/schema.sql", "utf8"));
    test(db)();
  } finally {
    db.close();
  }
};

// Seed the failure-then-retry history that exists from before retry admission
// was disabled. The public insert operation deliberately no longer permits it.
export const insertHistoricalPublishJob = (db) => (payload) => () => {
  const { name, version } = JSON.parse(payload);
  const jobId = randomUUID();
  const latest = db.prepare("SELECT MAX(createdAt) AS createdAt FROM job_info").get();
  const createdAt = new Date(Date.parse(latest.createdAt) + 1).toISOString();
  db.prepare("INSERT INTO job_info (jobId, createdAt) VALUES (?, ?)").run(jobId, createdAt);
  db.prepare("INSERT INTO publish_jobs (jobId, packageName, packageVersion, payload) VALUES (?, ?, ?, ?)")
    .run(jobId, name, version, payload);
  return jobId;
};
