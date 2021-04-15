import { createConnection } from "typeorm";
import { Project } from "./Project";
import { Article } from "./Article";
import { Page } from "./Page";
import { Tag } from "./Tag";
import { TimeSlug } from "./TimeSlug";

export default function createCacheConnection() {
  return createConnection({
    type: "sqlite",
    database: "data/content.sqlite3",
    logging: true,
    synchronize: true,
    entities: [Project, Article, Page, Tag, TimeSlug],
  });
}
