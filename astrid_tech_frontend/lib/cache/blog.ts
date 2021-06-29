import { AstridTechAPI, Entry, EntryDetail } from "lib/astrid-tech-api";
import { parse } from "remark";
import { BlogPost, BlogPostMeta } from "../../types/types";
import { getConnection } from "./util";

export type Path = {
  year: string;
  month: string;
  day: string;
  slug: string[];
};

export function getPathFromEntry(entry: Entry) : Path{
  return buildPath(new Date(entry.date), entry.ordinal, entry.slug_name)
}

export function getYMDString(date: Date) {
  const year = date.getFullYear().toString();
  const month = (date.getMonth() + 1).toString().padStart(2, "0");
  const day = (date.getDate() + 1).toString().padStart(2, "0");
  return { year, month, day };
}

export function buildPath(
  date: Date,
  ordinal: number,
  slugName?: string
): Path {
  const { year, month, day } = getYMDString(date);

  const slug = [ordinal.toString()];
  if (slugName && slugName.length > 0) {
    slug.push(slugName);
  }
  return { year, month, day, slug };
}

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
    return buildPath(new Date(e.date), e.ordinal, e.slug_name);
  });
}

export async function resolveBlogPost(
  year: string,
  month?: string,
  day?: string,
  ordinal?: string
): Promise<Entry[]> {
  const parsed = [];
  for (const s of [year, month, day, ordinal]) {
    const n = parseInt(s);
    if (isNaN(n)) {
      break;
    }
    parsed.push(n);
  }

  const params = (() => {
    const [year, month, day, ordinal] = parsed as [
      number,
      number?,
      number?,
      number?
    ];
    const params = {} as any;
    if (year) params.year = year;
    if (month) params.month = month;
    if (day) params.day = day;
    if (ordinal) params.ordinal = ordinal;
    return params;
  })();

  console.log("Resolving params", params);

  const api = AstridTechAPI.createWithEnvRoot();
  return await api.getEntries(params);
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
