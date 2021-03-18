var ids = require("spdx-license-ids");
var deprecatedIds = require("spdx-license-ids/deprecated");

var identifiers = ids.concat(deprecatedIds);

exports.isValidSPDXLicenseId = function (identifier) {
  return identifiers.includes(identifier);
};
