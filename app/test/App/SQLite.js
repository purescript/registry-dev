import Database from "better-sqlite3";
import { readFileSync } from "node:fs";

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
