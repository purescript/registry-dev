const semver = require("semver");

exports.parseSemVerImpl = (input) => {
  try {
    return semver.parse(input);
  } catch (e) {
    return null;
  }
};

exports.compareSemVerImpl = semver.compare;

exports.parseRangeImpl = semver.validRange;
