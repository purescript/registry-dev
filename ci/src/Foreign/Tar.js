const tar = require("tar");

// get toplevel directory in tar
exports.getToplevelDirImpl = function (filename) {
  return function () {
    var entries = [];
    tar.list({
      file: filename,
      sync: true,
      filter: (path, entry) => {
        var topLevel = /^[^\/]+\/$/;
        return topLevel.exec(path);
      },
      onentry: (entry) => {
        entries.push(entry.path);
      },
    });
    return entries;
  };
};

// extract tar
exports.extractImpl = function (cwd, filename) {
  return function () {
    tar.extract({
      sync: true,
      cwd: cwd,
      file: filename,
    });
  };
};

// create .tar.gz from a folder
exports.createImpl = function (cwd, foldername, archivename) {
  return function () {
    tar.create(
      {
        sync: true,
        gzip: true,
        portable: true,
        noMtime: true,
        cwd: cwd,
        filter: (path, stat) => {
          // 'no mtime' and 'portable' aren't enough on their own:
          // https://github.com/npm/node-tar/issues/176#issuecomment-391904257
          stat.mtime = null;
          stat.birthtime = null;
        },
        file: archivename,
      },
      [foldername]
    );
  };
};
