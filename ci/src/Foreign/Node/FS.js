const fs = require("fs");

exports.mkdirSyncImpl = (path) => fs.mkdirSync(path, { recursive: true });

exports.rmdirSyncImpl = (path) => fs.rmdirSync(path, { recursive: true });
