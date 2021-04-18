export { Article } from "./Article";
export { Page } from "./Page";
export { Project } from "./Project";
export { Tag } from "./Tag";
export { TimeSlug } from "./TimeSlug";
export { Note } from "./Note";
//export { Recipe } from "./Recipe";
import createCacheConnection from "./connection";

export const createConnection = createCacheConnection;
