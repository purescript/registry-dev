const fs = require("fs-extra");

exports.ensureDirectoryImpl = (path) => () => fs.ensureDir(path);

exports.removeImpl = (path) => () => fs.remove(path);

exports.copyImpl = (source) => (destination) => (options) => () =>
  fs.copy(source, destination, options);
