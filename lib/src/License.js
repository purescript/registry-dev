import parse from "spdx-expression-parse";
import currentIds from "spdx-license-ids/index.json" with { type: "json" };
import deprecatedIds from "spdx-license-ids/deprecated.json" with { type: "json" };

export { currentIds, deprecatedIds };

const toTree = (onError, node, onLeaf, onAnd, onOr) => {
  if (node.license != null) {
    const plus = node.plus === true;
    const exception = node.exception ?? "";
    return onLeaf(node.license)(plus)(exception);
  }

  if (node.left != null && node.right != null) {
    const left = toTree(onError, node.left, onLeaf, onAnd, onOr);
    const right = toTree(onError, node.right, onLeaf, onAnd, onOr);

    if (node.conjunction === "and") {
      return onAnd(left)(right);
    }

    if (node.conjunction === "or") {
      return onOr(left)(right);
    }

    return onError(`Unsupported SPDX conjunction '${String(node.conjunction)}'`);
  }

  return onError("Unsupported SPDX AST node");
};

export const parseLicenseTreeImpl = (onError, onLeaf, onAnd, onOr, expression) => {
  try {
    const ast = parse(expression);
    return toTree(onError, ast, onLeaf, onAnd, onOr);
  } catch (error) {
    const message = error instanceof Error ? error.message : String(error);
    return onError(`Invalid SPDX expression '${expression}': ${message}`);
  }
};
