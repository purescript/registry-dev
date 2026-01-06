export const log = s => () => console.log(s);
export const warn = s => () => console.warn(s);
export const error = s => () => console.error(s);
export const info = s => () => console.info(s);
export const debug = s => () => console.debug(s);
export const time = s => () => console.time(s);
export const timeLog = s => () => console.timeLog(s);
export const timeEnd = s => () => console.timeEnd(s);
export const clear = () => console.clear();
