const tmp = require('tmp');

tmp.setGracefulCleanup();

exports.mkTmpDirImpl = function() {
  return function() {
    const tmpobj = tmp.dirSync();
    return tmpobj.name;
  };
};


// TODO: we should do proper bracketing when creating the temporary directory
// so that if the program crashes we still remove it.
// In practice we don't really care since this thing is going to run in CI,
// so this is more of a nice to have for when we run it locally.
// To remove the temp directory we can call tmpobj.removeCallback();
