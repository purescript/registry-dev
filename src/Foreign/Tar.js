import { list, extract, create } from "tar";

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
    onentry: (entry) => {
      entries.push(entry.path);
    },
  });
  return entries;
};

// extract tar
export const extractImpl = (cwd, filename) => () => {
  extract({
    sync: true,
    cwd: cwd,
    file: filename,
  });
};

// create .tar.gz from a folder
export const createImpl = (cwd, foldername, archivename) => () => {
  create(
    {
      sync: true,
      gzip: true,
      portable: true,
      cwd: cwd,
      file: archivename,
    },
    [foldername]
  );
};
