import path from "path";
import fs from "fs-extra";
import { createCacheConnection } from "../lib/db";
import { buildArticleCache } from "./article";
import { buildProjectCache } from "./projects";
import { loadTags } from "./tags";
import { deleteObjects } from "./util";

const contentDir = path.join(process.cwd(), "content");

async function main() {
  await deleteObjects();
  await fs.mkdir(contentDir);

  const connection = await createCacheConnection();

  await loadTags(connection, path.join(contentDir, "tags"));
  await buildProjectCache(connection, contentDir);
  await buildArticleCache(connection, contentDir);

  await connection.close();
}

main().catch((e) => {
  throw e;
});
