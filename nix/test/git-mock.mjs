#!/usr/bin/env node

/*

Mock git binary for testing. Detects arguments to 'git' containing a URL
and replaces them with a local filepath. This is a drop-in replacement
for 'git' that should be used in offline / test environments when we only
want fixture data.

*/

import { spawn } from "node:child_process";

const repoFixturesDir = process.env.REPO_FIXTURES_DIR;
if (!repoFixturesDir) {
  throw new Error("REPO_FIXTURES_DIR is not set, but is required.");
}

const gitBinary = process.env.GIT_BINARY;
if (!gitBinary) {
  throw new Error("GIT_BINARY is not set, but is required.");
}

// Replace any URL arguments with the local fixtures path.
function replaceIfUrl(arg) {
  try {
    const url = new URL(arg);
    const path = url.pathname.replace(/\.git$/, "");
    const file = "file://" + repoFixturesDir + path;
    console.log(file);
    return file;
  } catch (e) {
    // Not a URL, ignore
  }
  return arg;
}

const args = process.argv.slice(2);
const modified = args.map(replaceIfUrl);

const git = spawn(gitBinary, modified);

git.stdout.on("data", (data) => {
  console.log(data.toString("utf8"));
});

git.stderr.on("data", (data) => {
  console.error(data.toString("utf8"));
});

git.on("close", (code) => {
  process.exit(code);
});
