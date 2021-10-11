const semver = require("semver");

exports.parseSemVerImpl = (input) => {
  try {
    return semver.parse(input);
  } catch (e) {
    return null;
  }
};

exports.versionImpl = function (sv) { return sv.version; }
exports.majorImpl = function (sv) { return sv.major; }
exports.minorImpl = function (sv) { return sv.minor; }
exports.prereleaseImpl = function (sv) { return sv.prerelease; }
exports.buildImpl = function (sv) { return sv.build; }
exports.patchImpl = function (sv) { return sv.patch; }

exports.compareSemVerImpl = semver.compare;

exports.parseRangeImpl = semver.validRange;
