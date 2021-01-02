import { SiteObject } from "../../types/types";
import { excerptify } from "../markdown";
import { getBlogPosts } from "./blog";
import { getProjects } from "./project";
import { getConnection } from "./util";

export function getTags(): string[] {
  const db = getConnection();
  const results = db
    .prepare(
      `SELECT DISTINCT tag FROM project_tag
      UNION
      SELECT DISTINCT tag from blog_tag`
    )
    .all();

  return results.map(({ tag }) => tag as string);
}

function getSortKey(object: SiteObject): number {
  switch (object.type) {
    case "b":
      return new Date(object.date).getTime();
    case "p":
      const now = new Date().getTime();
      return object.endDate
        ? (now + new Date(object.endDate).getTime()) / 2
        : now;
  }
  throw new Error("Unsupported type");
}

export async function getTagged(
  tag: string,
  excerptChars: number
): Promise<SiteObject[]> {
  const [projects, posts] = await Promise.all([
    Promise.all(
      getProjects()
        .map((x) => ({ type: "p", ...x }))
        .map(excerptify(excerptChars))
    ),
    Promise.all(
      getBlogPosts()
        .map((x) => ({ type: "b", ...x }))
        .map(excerptify(excerptChars))
    ),
  ]);
  const objects = (projects as SiteObject[]).concat(posts as SiteObject[]);

  return objects
    .filter((x) => x.tags.includes(tag))
    .sort((a, b) => {
      return getSortKey(b) - getSortKey(a);
    });
}
