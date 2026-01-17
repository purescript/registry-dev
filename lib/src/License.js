import parse from "spdx-expression-parse";

export const parseSPDXLicenseIdImpl = (onError, onSuccess, identifier) => {
  try {
    parse(identifier);
    return onSuccess(identifier);
  } catch (_) {
    return onError(`Invalid SPDX identifier ${identifier}`);
  }
};

// Extract all license IDs from a parsed SPDX expression AST.
// The AST structure from spdx-expression-parse is:
// - Simple: { license: 'MIT' }
// - With exception: { license: 'GPL-2.0', exception: 'Classpath-exception-2.0' }
// - Compound: { left: {...}, conjunction: 'and'|'or', right: {...} }
const extractLicenseIds = (ast) => {
  const ids = new Set();

  const walk = (node) => {
    if (!node) return;
    if (node.license) {
      // Normalize to uppercase for case-insensitive comparison
      ids.add(node.license.toUpperCase());
    }
    if (node.left) walk(node.left);
    if (node.right) walk(node.right);
  };

  walk(ast);
  return Array.from(ids);
};

export const extractLicenseIdsImpl = (onError, onSuccess, expression) => {
  try {
    const ast = parse(expression);
    return onSuccess(extractLicenseIds(ast));
  } catch (_) {
    return onError(`Invalid SPDX expression: ${expression}`);
  }
};
