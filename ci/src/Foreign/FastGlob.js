const fg = require("fast-glob");

exports.unsafeMatchImpl = (entries) => (options) => () =>
  fg(entries, options).then((matches) => {
    return matches.map((match) => {
      return { path: match.path, isSymbolicLink: match.stats.isSymbolicLink() };
    });
  });
