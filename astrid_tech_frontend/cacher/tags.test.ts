import { cleanUpCache, getResource, setUpCache } from "./test-util";
import { getConnection } from "typeorm";
import { createCacheConnection, Tag } from "../lib/db";
import { assert } from "chai";
import { loadLanguageTags, loadTags, readUserTagFile } from "./tags";

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

  describe("loadLanguageTags", () => {
    it("should work", async () => {
      const result = await loadLanguageTags();

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

      assert(result.find((x) => x.shortName == "aws"));
      assert(result.find((x) => x.shortName == "jupyter"));
      assert(result.find((x) => x.shortName == "material-ui"));
      assert(conn.getRepository(Tag).findOne({ shortName: "backend" }));
      assert(conn.getRepository(Tag).findOne({ shortName: "numpy" }));
      assert(conn.getRepository(Tag).findOne({ shortName: "gcloud" }));
    });
  });
});
