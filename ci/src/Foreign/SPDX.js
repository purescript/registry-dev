var parse = require("spdx-expression-parse");
var ids = require("spdx-license-ids");
var deprecatedIds = require("spdx-license-ids/deprecated");
var Fuse = require("fuse.js");

const identifiers = ids.concat(deprecatedIds);

const fuse = new Fuse(identifiers);

exports.parseSPDXLicenseIdImpl = function (onError, onSuccess, identifier) {
  try {
    parse(identifier);
    return onSuccess(identifier);
  } catch (_) {
    const results = fuse.search(identifier);
    if (results.length < 1) {
      return onError(`Invalid SPDX identifier: ${identifier}.`);
    } else {
      return onError(
        `Invalid SPDX identifier: ${identifier}. Did you mean ${results[0].item}?`
      );
    }
  }
};
