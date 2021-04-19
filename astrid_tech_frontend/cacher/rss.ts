import { Feed } from "feed";
import { promises as fs } from "fs";
import path from "path";
import { convertBlogPostToObjectDate } from "../types/types";
import { Connection } from "typeorm";
import { Page } from "lib/db";

async function createRSSFeed(conn: Connection, hostname: string) {
  const root = `https://${hostname}`;
  const author = {
    name: "Astrid Yu",
    email: "astrid@astrid.tech",
    link: `${root}`,
  };
  const feed = new Feed({
    title: "astrid.tech",
    description: "Astrid Yu's tech blog",
    id: root,
    link: root,
    language: "en",
    image: root,
    favicon: root,
    copyright: "All rights reserved 2021, Astrid Yu",
    feedLinks: {
      json: `${root}/feed.json`,
      atom: `${root}/atom.xml`,
      rss: `${root}/rss.xml`,
    },
    author,
  });

  feed.addCategory("Technology");

  (
    await conn.getRepository(Page).find({
      order: { date: "DESC" },
    })
  ).map((post) => {
    const url = root + post.pathname;
    return feed.addItem({
      title: post.title,
      id: url,
      link: url,
      date: post.date,
      description: post.description,
      content: post.contentMarkdown,
      author: [author],
      image: post.thumbnail,
      category: post.tags.map((t) => ({ name: t.shortName })),
    });
  });

  return feed;
}

async function writeFeedData(assetRoot: string, feed: Feed) {
  await Promise.all([
    fs.writeFile(path.join(assetRoot, "rss.xml"), feed.rss2()),
    fs.writeFile(path.join(assetRoot, "atom.xml"), feed.atom1()),
    fs.writeFile(path.join(assetRoot, "feed.json"), feed.json1()),
  ]);
}

export async function buildRSSFeed(conn: Connection, assetRoot: string) {
  console.log("Building RSS, Atom, and JSON feeds");
  const feed = await createRSSFeed(conn, "astrid.tech");
  await writeFeedData(assetRoot, feed);
}
