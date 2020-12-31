import { BlogPost, BlogPostMeta } from "../../types/types";
import { getMarkdownExcerpt } from "../markdown";
import { getBlogSlug } from "../util";
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
  return results.map((post) =>
    getBlogSlug({
      date: new Date(post.dateStr),
      slug: post.slug,
    })
  );
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

export async function getBlogPostMetas(
  excerptMaxChars: number
): Promise<BlogPostMeta<string>[]> {
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

  const kvs = await Promise.all(
    rows.map((row) =>
      (async () =>
        [
          row.id,
          {
            ...row,
            content: null,
            excerpt: await getMarkdownExcerpt(row.content, excerptMaxChars),
            tags: [],
          },
        ] as [number, BlogPostMeta<string>])()
    )
  );

  const idToPost = new Map(kvs);

  for (const { fk_blog, tag } of tags) {
    idToPost.get(fk_blog)!!.tags.push(tag as string);
  }

  return [...idToPost.values()];
}
