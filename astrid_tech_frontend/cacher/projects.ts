import { promises as fs } from "fs";
import matter from "gray-matter";
import path, { join, ParsedPath, relative } from "path";
import { Connection } from "typeorm";
import { Project, ProjectStatus } from "../types/types";
import { walkArr } from "./util";
import * as db from "../lib/db";

function getShortName(parsed: ParsedPath) {
  return parsed.name == "index" ? path.parse(parsed.dir).name : parsed.name;
}

export async function loadProject({
  assetRoot,
  pathname,
}: {
  assetRoot: string;
  pathname: string;
}): Promise<db.Project> {
  const parsed = path.parse(pathname);
  const shortName = getShortName(parsed);
  const fileContents = await fs.readFile(pathname);
  const { data, content } = matter(fileContents);

  const startDate = new Date(data.startDate);

  const page: db.Page = {
    title: data.title,
    thumbnail: data.thumbnail,
    contentMarkdown: content,
    date: startDate,
    pathname: `/projects/${shortName}`,
    assetRoot: assetRoot,
    tags: data.tags,
    objectType: "project",
  };

  return {
    shortName,
    status: data.status ? (data.status as ProjectStatus) : null,
    startDate: startDate,
    endDate: data.endDate ? new Date(data.endDate) : null,
    page: page,
    url: data.url,
    source: data.source ? data.source : [],
    description: data.description,
  };
}

export async function getProjects(contentDir: string): Promise<db.Project[]> {
  const dirs = await walkArr(join(contentDir, "projects"));
  return Promise.all(
    dirs
      .filter(({ stats }) => stats.isFile() && stats.name.endsWith(".md"))
      .map(({ root, stats }) =>
        loadProject({
          pathname: join(root, stats.name),
          assetRoot: relative(contentDir, root),
        })
      )
  );
}

export async function buildProjectCache(contentDir: string, conn: Connection) {
  console.log("Building project cache");

  const projects = await getProjects(contentDir);

  await conn
    .createQueryBuilder()
    .insert()
    .into(db.Project)
    .values(projects)
    .execute();
}
