import path from "path";
import { createCacheConnection } from "../lib/db/index";
import { loadProject } from "./projects";
import assert from "assert";
import { TEST_RESOURCES } from "./test-utils";

before(async () => {
  await createCacheConnection();
});

describe("loadProject", () => {
  it("should parse a file", async () => {
    const result = await loadProject({
      assetRoot: "/foo/bar/spam",
      pathname: path.join(
        TEST_RESOURCES,
        "content/2020-sample/projects/astrid-tech/index.md"
      ),
    });

    assert.strictEqual(result.shortName, "astrid-tech");
  });
});
