import walk from "walk";
import path from "path";
import rimraf from "rimraf";
import * as db from "../lib/db";
import { Connection, getConnection } from "typeorm";

export async function walkArr<T>(dir: string) {
  const out: { root: string; stats: walk.WalkStats }[] = [];
  await new Promise<void>((resolve, reject) => {
    const walker = walk.walk(dir);
    walker.on("file", async (root, stats, next) => {
      out.push({ root, stats });
      next();
    });

    walker.on("errors", (root, nodeStatsArray) =>
      reject({ root, nodeStatsArray })
    );

    walker.on("end", resolve);
  });
  return out;
}

export function serializeJS(data: any, msg?: string) {
  return `/* This is an AUTO-GENERATED FILE. ${msg ?? ""} */
/* eslint-disable */
// prettier-ignore
module.exports=${JSON.stringify(data)}`;
}

export async function deleteObjects() {
  // see https://stackoverflow.com/questions/58779347/jest-typeorm-purge-database-after-all-tests

  await getConnection().transaction(async (em) => {
    for (const entity of getConnection().entityMetadatas) {
      const repository = em.getRepository(entity.name); // Get repository
      await repository.clear(); // Clear each entity table's content
    }
  });
}

export function clearCaches() {
  const dataDir = path.join(__dirname, "../data");
  rimraf(dataDir, console.error);
}

export function loadTagList(
  conn: Connection,
  tags: string[]
): Promise<db.Tag[]> {
  return Promise.all(
    tags.map((shortName: string) => db.getOrCreateTag(conn, { shortName }))
  );
}
