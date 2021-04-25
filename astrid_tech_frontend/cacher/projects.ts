import fs from "fs-extra";
import matter from "gray-matter";
import path, { join, ParsedPath, relative } from "path";
import { Connection } from "typeorm";
import { Project, ProjectStatus } from "../types/types";
import { loadTagList, walkArr } from "./util";
import * as db from "../lib/db";

function getShortName(parsed: ParsedPath) {
  return parsed.name == "index" ? path.parse(parsed.dir).name : parsed.name;
}

export async function loadProject({
  conn,
  assetRoot,
  pathname,
}: {
  conn: Connection;
  assetRoot: string;
  pathname: string;
}): Promise<db.Project> {
  const parsed = path.parse(pathname);
  const shortName = getShortName(parsed);
  const fileContents = await fs.readFile(pathname);
  const { data, content } = matter(fileContents);

  const startDate = new Date(data.startDate);
  const tags: db.Tag[] = await loadTagList(conn, data.tags);

  const pageRepo = conn.getRepository(db.Page);
  const page = await pageRepo.save({
    title: data.title,
    description: data.description,
    thumbnail: data.thumbnail,
    contentMarkdown: content,
    date: startDate,
    pathname: `/projects/${shortName}`,
    assetRoot: assetRoot,
    tags,
    objectType: "project",
  });

  await db.getOrCreateTag(conn, { shortName: `project:${shortName}` });

  return conn.getRepository(db.Project).save({
    shortName,
    status: data.status as ProjectStatus,
    startDate: startDate,
    endDate: data.endDate ? new Date(data.endDate) : undefined,
    page: page,
    url: data.url ?? [],
    source: data.source ? data.source : [],
  });
}

export async function buildProjectCache(conn: Connection, contentDir: string) {
  const dirs = await walkArr(join(contentDir, "projects"));

  await Promise.all(
    dirs
      .filter(({ stats }) => stats.isFile() && stats.name.endsWith(".md"))
      .map(({ root, stats }) =>
        loadProject({
          conn,
          pathname: join(root, stats.name),
          assetRoot: relative(contentDir, root),
        })
      )
  );
}
