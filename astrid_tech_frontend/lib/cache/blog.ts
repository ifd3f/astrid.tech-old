import { excerptify } from "lib/markdown";
import { BlogPost, BlogPostMeta } from "../../types/types";
import { getBlogSlug } from "../util";
import { getConnection } from "./util";

export type Path = {
  year: string;
  month: string;
  day: string;
  ordinal: string;
  slug?: string;
};

export type FQPath = Path & {
  slug: string;
};

export function rowToBlogMeta(row: any, tags: string[]): BlogPostMeta<string> {
  return {
    ...row,
    thumbnail: null,
    tags,
  };
}

export function rowToBlogPost(row: any, tags: string[]): BlogPost<string> {
  return {
    ...rowToBlogMeta(row, tags),
    content: row.content as string,
  };
}

export function getBlogPostSlugs(): FQPath[] {
  const db = getConnection();
  const results = db
    .prepare("SELECT date AS dateStr, ordinal, slug FROM blog_post")
    .all() as {
    dateStr: string;
    ordinal: number;
    slug: string;
  }[];

  return results.map((post) =>
    getBlogSlug({
      date: new Date(post.dateStr),
      ordinal: post.ordinal,
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
        ordinal,
        content
      FROM blog_post
      WHERE 
        year = @year AND 
        month = @month AND 
        day = @day AND 
        ordinal = @ordinal`
    )
    .get({
      year: path.year,
      month: path.month,
      day: path.day,
      ordinal: path.ordinal,
    });

  const tags = db
    .prepare(`SELECT tag FROM blog_tag WHERE fk_blog = @id`)
    .all({ id: row.id })
    .map(({ tag }) => tag);
  return rowToBlogPost(row, tags);
}

export function getBlogPosts(): BlogPost<string>[] {
  const db = getConnection();
  const rows = db
    .prepare(
      `SELECT 
        id, 
        asset_root as assetRoot, 
        thumbnail, 
        title, 
        description, 
        date, 
        ordinal,
        slug, 
        content
      FROM blog_post
      ORDER BY date DESC`
    )
    .all();

  const tags = db.prepare(`SELECT fk_blog, tag FROM blog_tag`).all();

  const idToPost = new Map(
    rows.map(
      (row) =>
        [
          row.id,
          {
            ...row,
            tags: [],
          },
        ] as [number, BlogPost<string>]
    )
  );

  for (const { fk_blog, tag } of tags) {
    idToPost.get(fk_blog)!!.tags.push(tag as string);
  }

  return [...idToPost.values()];
}
