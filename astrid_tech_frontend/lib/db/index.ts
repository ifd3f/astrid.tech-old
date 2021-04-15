export { Article } from "./Article";
export { Page } from "./Page";
export { Project } from "./Project";
export { Tag } from "./Tag";
export { TimeSlug } from "./TimeSlug";
import createCacheConnection from "./connection";

export const createConnection = createCacheConnection;
