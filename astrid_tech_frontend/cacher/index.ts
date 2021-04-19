import path from "path";
import { createCacheConnection } from "../lib/db";
import { buildArticleCache } from "./article";
import { buildProjectCache } from "./projects";
import { loadTags } from "./tags";
import { clearCaches } from "./util";

const contentDir = path.join(process.cwd(), "content");

async function main() {
  clearCaches();

  const connection = await createCacheConnection();

  await loadTags(connection, path.join(contentDir, "tags"));
  await buildProjectCache(connection, contentDir);
  await buildArticleCache(connection, contentDir);

  await connection.close();
}

main().catch((e) => {
  throw e;
});
