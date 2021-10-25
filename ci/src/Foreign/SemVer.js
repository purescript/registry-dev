const semver = require("semver");

exports.parseSemVerImpl = (input) => {
  try {
    return semver.parse(input);
  } catch (e) {
    return null;
  }
};

exports.version = function (sv) { return sv.version; }
exports.major = function (sv) { return sv.major; }
exports.minor = function (sv) { return sv.minor; }
exports.prerelease = function (sv) { return sv.prerelease; }
exports.build = function (sv) { return sv.build; }
exports.patch = function (sv) { return sv.patch; }

exports.compareSemVerImpl = semver.compare;

exports.parseRangeImpl = semver.validRange;

exports.maxSatisfyingImpl = semver.maxSatisfying;
