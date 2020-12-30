import { promises as fs } from "fs";
import matter from "gray-matter";
import path, { join, ParsedPath } from "path";
import { AssetDirObject } from "../lib/util";
import { Project, ProjectStatus } from "../types/types";
import { walkArr } from "./util";

function getSlug(parsed: ParsedPath) {
  return parsed.name == "index" ? path.parse(parsed.dir).name : parsed.name;
}

async function loadProject(pathname: string): Promise<Project> {
  const parsed = path.parse(pathname);
  const slug = getSlug(parsed);
  const fileContents = await fs.readFile(pathname);
  const { data, content } = matter(fileContents);

  return {
    title: data.title,
    status: data.status ? (data.status as ProjectStatus) : null,
    tags: data.tags,
    startDate: new Date(data.startDate),
    endDate: data.endDate ? new Date(data.endDate) : null,
    slug,
    url: data.url,
    source: data.source ? data.source : [],
    thumbnail: data.thumbnail,
    content: content,
    description: data.description,
  };
}

export async function getProjects(
  contentDir: string
): Promise<Promise<AssetDirObject<Project<Date>>>[]> {
  const files = (await walkArr(join(contentDir, "projects")))
    .filter(({ stats }) => stats.isFile() && stats.name.endsWith(".md"))
    .map(({ root, stats }) =>
      loadProject(join(root, stats.name)).then((project) => ({
        assetRoot: root,
        object: project,
      }))
    );
  return files;
}
