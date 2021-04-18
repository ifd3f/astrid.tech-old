import { createCacheConnection } from "../lib/db/index";
import { buildProjectCache, loadProject } from "./projects";
import { getResource } from "./test-utils";
import { getConnection } from "typeorm";
import { Tag } from "../lib/db/Tag";
import { expect, assert } from "chai";

before(async () => {
  await createCacheConnection();
});

describe("loadProject", () => {
  it("should parse a valid project file", async () => {
    const result = await loadProject({
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

    await buildProjectCache(getResource("content/2020-sample"), conn);

    const newTag = await conn.getRepository(Tag).findOne({ name: "react-js" });
    assert(newTag, "no tag was generated");
  });
});
