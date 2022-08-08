var crypto = require("crypto");

exports.createHash = (algorithm) => () => crypto.createHash(algorithm);

exports.updateHash = (buffer) => (hash) => () => hash.update(buffer);

exports.digestHash = (hash) => () => hash.digest();
