import { Feed, Item } from "feed";
import { promises as fs } from "fs";
import { getBlogPosts } from "../lib/cache";
import path from "path";
import { BlogPost } from "../types/types";
import { blogSlugToString, getBlogSlug } from "../lib/util";
import { convertBlogPostToObjectDate } from "../types/types";
import { renderMarkdown } from "../lib/markdown";

async function createRSSFeed(hostname: string, posts: BlogPost[]): Promise<Feed> {
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
  const generatedItems = await Promise.all(
    posts.map(async (post) => {
      const url = root + blogSlugToString(getBlogSlug(post));
      const content = await renderMarkdown(post.content, post.assetRoot);
      const category = post.tags.map((t) => ({ name: t }));

      return {
        title: post.title,
        id: url,
        link: url,
        date: post.date,
        description: post.description,
        content,
        author: [author],
        category,
      } as Item;
    })
  );

  generatedItems.forEach(feed.addItem);

  return feed;
}

async function writeFeedData(assetRoot: string, feed: Feed) {
  await Promise.all([
    fs.writeFile(path.join(assetRoot, "rss.xml"), feed.rss2()),
    fs.writeFile(path.join(assetRoot, "atom.xml"), feed.atom1()),
    fs.writeFile(path.join(assetRoot, "feed.json"), feed.json1()),
  ]);
}

export async function buildRSSFeed(assetRoot: string) {
  console.log("Building RSS, Atom, and JSON feeds");
  const posts = getBlogPosts().map((p) => convertBlogPostToObjectDate(p));
  const feed = await createRSSFeed("astrid.tech", posts);
  await writeFeedData(assetRoot, feed);
}
