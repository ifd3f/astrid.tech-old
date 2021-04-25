import { createCacheConnection, Tag } from "../lib/db";
import path from "path";
import { Connection, getConnection } from "typeorm";
import { clearCaches, deleteObjects } from "./util";
import { assert } from "chai";

export function getResource(subpath: string) {
  return path.join(__dirname, "../test-resources", subpath);
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
