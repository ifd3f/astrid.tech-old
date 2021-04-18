import { createCacheConnection } from "../lib/db/index";
import { buildProjectCache, loadProject } from "./projects";
import { getResource } from "./test-utils";
import { getConnection } from "typeorm";
import { Tag } from "../lib/db";
import { expect, assert } from "chai";

before(async () => {
  await createCacheConnection();
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
    const newTag = await conn
      .getRepository(Tag)
      .findOne({ shortName: "react-js" });
    assert(newTag, "no tag was generated");
  });
});

describe("buildProjectCache", () => {
  it("should build cache for project files", async () => {
    const conn = getConnection();

    await buildProjectCache(conn, getResource("content/2020-sample"));

    const newTag = await conn
      .getRepository(Tag)
      .findOne({ shortName: "react-js" });
    assert(newTag, "no tag was generated");
  });
});
