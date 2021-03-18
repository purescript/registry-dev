var parse = require("spdx-expression-parse");

exports.isValidSPDXLicenseId = function (identifier) {
  try {
    parse(identifier);
    return true;
  } catch (e) {
    return false;
  }
};
