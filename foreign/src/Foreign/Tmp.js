import { setGracefulCleanup, dirSync } from "tmp";

setGracefulCleanup();

export const mkTmpDirImpl = () => {
  const tmpobj = dirSync({ template: 'XXXXXX' });
  return tmpobj.name;
};
