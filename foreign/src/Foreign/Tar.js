import { list } from "tar";

// get toplevel directory in tar
export const getToplevelDirImpl = (filename) => () => {
  var entries = [];
  list({
    file: filename,
    sync: true,
    filter: (path, _) => {
      var topLevel = /^[^\/]+\/$/;
      return topLevel.exec(path);
    },
    onReadEntry: (entry) => {
      entries.push(entry.path);
    },
  });
  return entries;
};
