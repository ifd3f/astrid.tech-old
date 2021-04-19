import {
  assertTagExists,
  cleanUpCache,
  getResource,
  setUpCache,
} from "./test-util";
import { getConnection } from "typeorm";
import { createCacheConnection, Tag } from "../lib/db";
import { assert } from "chai";
import { readLanguageTags, loadTags, readUserTagFile } from "./tags";

describe("Tag Import", async () => {
  before(async () => {
    await createCacheConnection();
  });

  beforeEach(async () => {
    await setUpCache();
  });

  afterEach(async () => {
    await cleanUpCache();
  });

  after(async () => {
    await getConnection().close();
  });

  describe("readLanguageTags", () => {
    it("should work", async () => {
      const result = await readLanguageTags();

      assert(result.find((x) => x.shortName == "cpp"));
      assert(result.find((x) => x.shortName == "tsv"));
    });
  });

  describe("readUserTagFile", () => {
    it("should read valid YAML files", async () => {
      const result = await readUserTagFile(
        getResource("content/2020-sample/tags/clouds.yaml")
      );

      assert(result.find((x) => x.shortName == "aws"));
    });
  });

  describe("loadTags", () => {
    it("should load tags from directory", async () => {
      const conn = getConnection();

      const result = await loadTags(
        conn,
        getResource("content/2020-sample/tags")
      );

      const repo = conn.getRepository(Tag);
      await assertTagExists(conn, "aws");
      await assertTagExists(conn, "jupyter");
      await assertTagExists(conn, "material-ui");
      await assertTagExists(conn, "backend");
      await assertTagExists(conn, "gcloud");
      await assertTagExists(conn, "numpy");
    });
  });
});
