import { inspect } from "node:util";

export const warnError = (error) => () => console.warn(inspect(error, { depth: null }));
