import {
  BlogPost,
  convertBlogPostToObjectDate,
  getBlogPosts,
  getBlogShortLinkCode,
  getMarkdownExcerpt,
} from "@astrid.tech/node-lib";
import { Database } from "better-sqlite3";
import { promises as fs } from "fs";
import path from "path";

type TWTXTEntry = {
  date: Date;
  content: string;
};

async function createTWTXT(
  shortDomain: string,
  posts: BlogPost[]
): Promise<TWTXTEntry[]> {
  const promises = posts.map(async (post) => {
    const shortcode = getBlogShortLinkCode(post);
    const shortlink = `${shortDomain}/${shortcode}`;
    const suffix = ` | ${shortlink}`;

    if (post.title) {
      return {
        date: post.date,
        content: `Article: ${post.title}${suffix}`,
      } as TWTXTEntry;
    }

    const maxLength = 140 - suffix.length;
    const excerpt = await getMarkdownExcerpt(post.content, maxLength);
    const cleaned = excerpt.replace(/[\r\n]+/g, " ");
    return {
      date: post.date,
      content: cleaned + suffix,
    } as TWTXTEntry;
  });

  return Promise.all(promises);
}

async function writeTWTXT(assetRoot: string, entries: TWTXTEntry[]) {
  const content = entries
    .map(({ date, content }) => `${date.toISOString()}\t${content}`)
    .join("\n");
  await fs.writeFile(path.join(assetRoot, "tw.txt"), content);
}

export async function buildTWTXT(db: Database, assetRoot: string) {
  console.log("Building tw.txt");
  const posts = getBlogPosts(db).map((p) => convertBlogPostToObjectDate(p));
  const feed = await createTWTXT("aay.tw", posts);
  await writeTWTXT(assetRoot, feed);
}
