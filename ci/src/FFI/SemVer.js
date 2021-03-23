const semver = require('semver');

exports.parseSemVerImpl = semver.parse;

exports.compareSemVerImpl = semver.compare;

exports.parseRangeImpl = semver.validRange;
