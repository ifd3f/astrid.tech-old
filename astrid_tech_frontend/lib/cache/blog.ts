import { AstridTechAPI, Entry, EntryDetail } from "lib/astrid-tech-api";
import { BlogPost, BlogPostMeta } from "../../types/types";
import { getBlogSlug } from "../util";
import { getConnection } from "./util";

export type Path = { year: string; month: string; day: string; slug: string[] };

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

export async function getBlogPostSlugs(): Promise<Path[]> {
  const api = AstridTechAPI.createWithEnvRoot();
  const entries = await api.getEntries();

  return entries.map((e) => {
    const date = new Date(e.date);
    const year = date.getFullYear().toString();
    const month = (date.getMonth() + 1).toString();
    const day = (date.getDate() + 1).toString();
    const slug = [e.ordinal.toString()];
    if (e.slug_name && e.slug_name.length > 0) {
      slug.push(e.slug_name);
    }
    return { year, month, day, slug };
  });
}

export async function getBlogPost(path: Path): Promise<EntryDetail> {
  const api = AstridTechAPI.createWithEnvRoot();
  const {
    year,
    month,
    day,
    slug: [ordinal],
  } = path;

  const [entry] = await api.getEntriesWithDetail({
    year,
    month,
    day,
    ordinal: parseInt(ordinal),
  });

  return entry;
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
        slug, 
        date, 
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
