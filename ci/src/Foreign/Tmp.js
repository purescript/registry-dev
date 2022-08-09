import { setGracefulCleanup, dirSync } from "tmp";

setGracefulCleanup();

export function mkTmpDirImpl() {
  return function () {
    const tmpobj = dirSync();
    return tmpobj.name;
  };
}

// TODO: we should do proper bracketing when creating the temporary directory
// so that if the program crashes we still remove it.
// In practice we don't really care since this thing is going to run in CI,
// so this is more of a nice to have for when we run it locally.
// To remove the temp directory we can call tmpobj.removeCallback();
