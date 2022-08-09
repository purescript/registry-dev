import crypto from "crypto";

export const createHash = (algorithm) => () => crypto.createHash(algorithm);

export const updateHash = (buffer) => (hash) => () => hash.update(buffer);

export const digestHash = (hash) => () => hash.digest();
