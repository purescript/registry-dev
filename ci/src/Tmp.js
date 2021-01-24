const tmp = require('tmp');

tmp.setGracefulCleanup();

exports.mkTmpDirImpl = function() {
  return function() {
    const tmpobj = tmp.dirSync();
    return tmpobj.name;
  };
};


// TODO: tmpobj.removeCallback();