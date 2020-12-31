import { BlogPost, BlogPostMeta } from "../../types/types";
import { getConnection } from "./util";

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
      month: (date.getMonth() + 1).toString().padStart(2, "0"),
      day: (date.getDate() + 1).toString().padStart(2, "0"),
      slug: [slug],
    };
  });
}

export function getBlogPost(path: Path): BlogPost<string> {
  const db = getConnection();
  const row = db
    .prepare(
      `SELECT 
        id, 
        asset_root as assetRoot, 
        thumbnail, 
        title, 
        description, 
        slug, 
        date, 
        content
      FROM blog_post
      WHERE year = @year AND month = @month AND day = @day AND slug = @slug`
    )
    .get({
      year: path.year,
      month: path.month,
      day: path.day,
      slug: path.slug[0],
    });

  const tags = db
    .prepare(`SELECT tag FROM blog_tag WHERE fk_blog = @id`)
    .all({ id: row.id });
  return {
    ...row,
    thumbnail: null,
    tags: tags.map(({ tag }) => tag),
  };
}

export function getBlogPostMetas(): BlogPostMeta<string>[] {
  const db = getConnection();
  const rows = db
    .prepare(
      `SELECT 
        id, 
        asset_root as assetRoot, 
        thumbnail, 
        title, 
        description, 
        slug, 
        date, 
        content
      FROM blog_post`
    )
    .all();

  const tags = db.prepare(`SELECT fk_blog, tag FROM blog_tag`).all();

  const idToPost = new Map<number, BlogPostMeta<string>>(
    rows.map((row) => [
      row.id as number,
      {
        ...row,
        tags: [],
      },
    ])
  );

  for (const { fk_blog, tag } of tags) {
    idToPost.get(fk_blog)!!.tags.push(tag as string);
  }

  return [...idToPost.values()];
}
