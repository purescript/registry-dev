import zlib from "node:zlib";

export const compressImpl = (input, onError, onBuffer) =>
  zlib.gzip(input, (error, buffer) =>
    error !== null ? onError(error) : onBuffer(buffer)
  );

export const toRequestBodyImpl = (a) => a;
