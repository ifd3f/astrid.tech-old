import { createCacheConnection, Tag } from "../lib/db";
import path from "path";
import { Connection, getConnection } from "typeorm";
import { clearCaches } from "./util";
import { assert } from "chai";

export function getResource(subpath: string) {
  return path.join(__dirname, "../test-resources", subpath);
}

async function deleteObjects() {
  // see https://stackoverflow.com/questions/58779347/jest-typeorm-purge-database-after-all-tests

  await getConnection().transaction(async (em) => {
    for (const entity of getConnection().entityMetadatas) {
      const repository = em.getRepository(entity.name); // Get repository
      await repository.clear(); // Clear each entity table's content
    }
  });
}

export async function setUpCache() {
  await deleteObjects();
}

export async function cleanUpCache() {
  await deleteObjects();
}

export function useCache() {
  beforeEach(async () => {
    await setUpCache();
  });

  afterEach(async () => {
    await cleanUpCache();
  });
}

export async function assertTagExists(conn: Connection, shortName: string) {
  const found = await conn.getRepository(Tag).findOne({ shortName });
  assert(found, `tag ${shortName} was generated`);
  return found;
}
