import { Feed, Item } from "feed";
import { promises as fs } from "fs";
import path from "path";
import { Connection } from "typeorm";
import { Article, Note, Page } from "../lib/db";

function articleAsRSS(author: any, root: string, article: Article): Item {
  const post = article.page;
  const url = root + post.pathname;
  return {
    title: post.title,
    id: url,
    link: url,
    date: post.date,
    description: post.description,
    content: post.contentMarkdown,
    author: [author],
    image: post.thumbnail,
    category: post.tags.map((t) => ({ name: t.shortName })),
  };
}

function noteAsRSS(author: any, root: string, note: Note): Item {
  const post = note.page;
  const url = root + post.pathname;
  return {
    title: post.title,
    id: url,
    link: url,
    date: post.date,
    description: post.description,
    content: post.contentMarkdown,
    author: [author],
    image: post.thumbnail,
    category: post.tags.map((t) => ({ name: t.shortName })),
  };
}

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

  const getArticlesTask = conn
    .getRepository(Article)
    .find()
    .then((articles) =>
      articles.map((article) => articleAsRSS(author, root, article))
    );

  const getNotesTask = conn
    .getRepository(Note)
    .find()
    .then((notes) => notes.map((note) => noteAsRSS(author, root, note)));

  const items = (await Promise.all([getArticlesTask, getNotesTask])).flat();

  items.forEach(feed.addItem);

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
