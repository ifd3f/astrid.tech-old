import { cleanUpCache, getResource, setUpCache, useCache } from "./test-util";
import { getConnection } from "typeorm";
import { createCacheConnection, Tag } from "../lib/db";
import { expect, assert } from "chai";
import { getOrdinal, getShortName, loadArticle } from "./article";
import path from "path";
import { buildProjectCache } from "./projects";

describe("getOrdinal", () => {
  it("parses with index-based file", () => {
    const parsed = path.parse("/foo/bar/blog/2021/03/05/3/spam/index.md");

    const result = getOrdinal(parsed);

    expect(result).equal(3);
  });

  it("parses with non-index file", () => {
    const parsed = path.parse("/foo/bar/blog/2021/03/05/31/spam.md");

    const result = getOrdinal(parsed);

    expect(result).equal(31);
  });
});

describe("getShortName", () => {
  it("parses with index-based file", () => {
    const parsed = path.parse("/foo/bar/blog/2021/03/05/3/spam/index.md");

    const result = getShortName(parsed);

    expect(result).equal("spam");
  });

  it("parses with non-index file", () => {
    const parsed = path.parse("/foo/bar/blog/2021/03/05/3/memes.md");

    const result = getShortName(parsed);

    expect(result).equal("memes");
  });
});

describe("Article Import", async () => {
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

  describe("loadArticle", () => {
    it("should parse a valid article file", async () => {
      const conn = getConnection();

      const result = await loadArticle({
        conn,
        assetRoot: "/foo/bar/spam",
        pathname: getResource(
          "content/2020-sample/blog/2020/06/07/0/hello-world.md"
        ),
      });

      expect(result.slug.shortName).equal("hello-world");
      const newTag = await conn
        .getRepository(Tag)
        .findOne({ shortName: "react-js" });
      assert(newTag, "no tag was generated");
    });
  });

  describe("buildArticleCache", () => {
    it("should build cache for article files", async () => {
      const conn = getConnection();

      await buildProjectCache(conn, getResource("content/2020-sample"));
      const newTag = await conn
        .getRepository(Tag)
        .findOne({ shortName: "react-js" });
      assert(newTag, "no tag was generated");
    });
  });
});
