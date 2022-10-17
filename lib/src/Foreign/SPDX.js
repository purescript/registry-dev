import parse from "spdx-expression-parse";
import Fuse from "fuse.js";
import { createRequire } from "module";

// The spdx-license-ids project is just a set of JSON files, which can't be
// imported directly into ESM without import assertions to guide TypeScript.
const require = createRequire(import.meta.url);
const identifiers = require("spdx-license-ids");
const deprecatedIdentifiers = require("spdx-license-ids/deprecated");

const allIdentifiers = identifiers.concat(deprecatedIdentifiers);

const fuse = new Fuse(allIdentifiers);

export const parseSPDXLicenseIdImpl = (onError, onSuccess, identifier) => {
  try {
    parse(identifier);
    return onSuccess(identifier);
  } catch (_) {
    const results = fuse.search(identifier);
    if (results.length < 1) {
      return onError(`Invalid SPDX identifier: ${identifier}`);
    } else {
      return onError(
        `Invalid SPDX identifier: ${identifier}\nDid you mean ${results[0].item}?`
      );
    }
  }
};
