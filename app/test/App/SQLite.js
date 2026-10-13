import Database from "better-sqlite3";
import { readFileSync } from "node:fs";

export const openDatabase = () => {
  const db = new Database(":memory:");
  db.pragma("foreign_keys = ON");
  db.exec(readFileSync("db/schema.sql", "utf8"));
  return db;
};

export const closeDatabase = db => () => db.close();

export const withDatabase = (test) => () => {
  const db = openDatabase();
  try {
    test(db)();
  } finally {
    db.close();
  }
};
