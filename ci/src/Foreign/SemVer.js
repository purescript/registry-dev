const semver = require("semver");

exports.parseRangeImpl = (rangeString) =>
  semver.validRange(rangeString, { loose: true, includePrerelease: false });
