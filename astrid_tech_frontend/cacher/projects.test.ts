import { buildProjectCache, loadProject } from "./projects";
import {
  assertTagExists,
  cleanUpCache,
  getResource,
  setUpCache,
  useCache,
} from "./test-util";
import { getConnection } from "typeorm";
import { createCacheConnection, Tag } from "../lib/db";
import { expect, assert } from "chai";

describe("Project Import", async () => {
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

  describe("loadProject", () => {
    it("should parse a valid project file", async () => {
      const conn = getConnection();

      const result = await loadProject({
        conn,
        assetRoot: "/foo/bar/spam",
        pathname: getResource(
          "content/2020-sample/projects/astrid-tech/index.md"
        ),
      });

      expect(result.shortName).equal("astrid-tech");
    });
  });

  describe("buildProjectCache", () => {
    it("should build cache for project files", async () => {
      const conn = getConnection();

      await buildProjectCache(conn, getResource("content/2020-sample"));
      await assertTagExists(conn, "react-js");
    });
  });
});
