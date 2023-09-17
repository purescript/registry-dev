import fg from "fast-glob";

export const matchImpl = (entries) => (options) => () => fg(entries, options);

export const convertPathToPattern = (path) => fg.convertPathToPattern(path);
