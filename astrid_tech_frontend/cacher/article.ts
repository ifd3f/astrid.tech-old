import { promises as fs } from "fs";
import matter from "gray-matter";
import path, { join, ParsedPath, relative } from "path";
import { Connection } from "typeorm";
import { BlogPost } from "../types/types";
import { loadTagList, walkArr } from "./util";
import * as db from "../lib/db";

export function getShortName(parsed: ParsedPath) {
  return parsed.name == "index" ? path.parse(parsed.dir).name : parsed.name;
}

export function getOrdinal(parsed: ParsedPath) {
  const splitPath = parsed.dir.split(path.sep);
  const tailname =
    parsed.name == "index"
      ? splitPath[splitPath.length - 2]
      : splitPath[splitPath.length - 1];

  return parseInt(tailname);
}

export async function loadArticle({
  conn,
  assetRoot,
  pathname,
}: {
  conn: Connection;
  assetRoot: string;
  pathname: string;
}): Promise<db.Article> {
  const parsed = path.parse(pathname);
  const shortName = getShortName(parsed);
  const fileContents = await fs.readFile(pathname);
  const { data, content } = matter(fileContents);

  const date = new Date(data.date);

  const tags: db.Tag[] = await loadTagList(conn, data.tags);

  const timeSlug = await conn.getRepository(db.TimeSlug).save(
    db.TimeSlug.create({
      date,
      objectType: "blog",
      shortName,
      ordinal: 0,
    })
  );

  const page = await conn.getRepository(db.Page).save({
    title: data.title,
    thumbnail: data.thumbnail,
    contentMarkdown: content,
    date,
    pathname: db.timeSlugToString(timeSlug),
    assetRoot: assetRoot,
    tags,
    objectType: "project",
  });

  return await conn.getRepository(db.Article).save({
    page,
    slug: timeSlug,
  });
}

export async function buildArticleCache(
  conn: Connection,
  contentDir: string
): Promise<db.Article[]> {
  console.log("Building blog post cache");

  const searchRoot = join(contentDir, "blog");
  const dirs = await walkArr(searchRoot);
  return await Promise.all(
    dirs
      .filter(
        ({ stats }) =>
          stats.isFile() &&
          stats.name.endsWith(".md") &&
          !stats.name.endsWith(".note.md") &&
          !stats.name.endsWith(".recipe.md")
      )
      .map(({ root, stats }) =>
        loadArticle({
          conn,
          assetRoot: relative(searchRoot, root),
          pathname: join(root, stats.name),
        })
      )
  );
}
