const fs = require("fs");

exports.mkdirSyncImpl = (path) => fs.mkdirSync(path, { recursive: true });
