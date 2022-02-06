const fg = require("fast-glob");

exports.matchImpl = (entries) => (options) => () => fg(entries, options);
