import sqlite3 from "better-sqlite3";
import { BlogPostWithDir } from "../cacher/content";

function getConnection() {
  return sqlite3("content.sqlite3", {});
}

export type Path = { year: string; month: string; day: string; slug: string[] };

export function getBlogPostSlugs(): Path[] {
  const db = getConnection();
  const results = db
    .prepare("SELECT date AS dateStr, slug FROM blog_post")
    .all() as {
    dateStr: string;
    slug: string;
  }[];
  return results.map(({ dateStr, slug }) => {
    const date = new Date(dateStr);
    return {
      year: date.getFullYear().toString(),
      month: (date.getMonth() + 1).toString(),
      day: (date.getDate() + 1).toString(),
      slug: [slug],
    };
  });
}

export function getBlogPost(path: Path): BlogPostWithDir<string> {
  const db = getConnection();
  const { id, assetRoot, title, slug, date, content, description } = db
    .prepare(
      `SELECT id, asset_root as assetRoot, thumbnail, title, description, slug, date, content
      FROM blog_post
      WHERE @date = strftime('%Y %m %d', date) AND @slug = slug`
    )
    .get({
      date: `${path.year} ${path.month} ${path.day}`,
      slug: path.slug[0],
    });

  const tags = db
    .prepare(`SELECT tag FROM blog_tag WHERE fk_blog = @id`)
    .all({ id: id });
  return {
    assetRoot,
    post: {
      title: title as string,
      slug: slug as string,
      date: date as string,
      content: content as string,
      description: description as string,
      tags: tags.map(({ tag }) => tag),
    },
  };
}
