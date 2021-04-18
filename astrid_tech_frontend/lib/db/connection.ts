import { Connection, createConnection } from "typeorm";
import { Project } from "./Project";
import { Article } from "./Article";
import { Page } from "./Page";
import { Tag } from "./Tag";
import { TimeSlug } from "./TimeSlug";
import { Note } from "./Note";

var connection: Connection | null = null;

/**
 * Creates a connection to the cache and returns it.
 * @returns the connection to the cache
 */
export async function createCacheConnection() {
  return await createConnection({
    type: "sqlite",
    database: "data/content.sqlite3",
    logging: false,
    synchronize: true,
    entities: [Project, Article, Note, Page, Tag, TimeSlug],
  });
}
