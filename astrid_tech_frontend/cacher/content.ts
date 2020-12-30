import { promises as fs } from "fs";
import matter from "gray-matter";
import yaml from "js-yaml";
import path, { join, ParsedPath } from "path";
import walk from "walk";
import { BlogPost, Tag } from "../types/types";
import { getContrastingTextColor } from "./util";

const contentDir = join(process.cwd(), "content");

export async function walkArr<T>(dir: string) {
  const out: { root: string; stats: walk.WalkStats }[] = [];
  await new Promise<void>((resolve, reject) => {
    const walker = walk.walk(dir);
    walker.on("file", async (root, stats, next) => {
      out.push({ root, stats });
      next();
    });

    walker.on("errors", (root, nodeStatsArray) =>
      reject({ root, nodeStatsArray })
    );

    walker.on("end", resolve);
  });
  return out;
}

function getSlug(parsed: ParsedPath) {
  const tailname =
    parsed.name == "index" ? path.parse(parsed.dir).name : parsed.name;
  const oldStyleMatch = tailname.match(/\d{4}-\d{2}-\d{2}-(.+)/);
  if (oldStyleMatch) {
    return oldStyleMatch[1];
  }
  return tailname.slice(0, -3);
}

async function loadBlogPost(pathname: string): Promise<BlogPost> {
  const parsed = path.parse(pathname);
  const slug = getSlug(parsed);
  const fileContents = await fs.readFile(pathname);
  const { data, content } = matter(fileContents);

  return {
    tags: data.tags,
    slug,
    title: data.title,
    content: content,
    date: new Date(data.date),
    thumbnail: data.thumbnail ? join(parsed.dir, data.thumbnail) : null,
    description: data.description,
  };
}

export type BlogPostWithDir<DateType> = {
  post: BlogPost<DateType>;
  assetRoot: string;
};

export async function getBlogPosts(): Promise<
  Promise<BlogPostWithDir<Date>>[]
> {
  const files = (await walkArr(join(contentDir, "blog")))
    .filter(({ stats }) => stats.isFile() && stats.name.endsWith(".md"))
    .map(({ root, stats }) =>
      loadBlogPost(join(root, stats.name)).then(
        (post) =>
          ({
            assetRoot: root,
            post,
          } as BlogPostWithDir<Date>)
      )
    );
  return files;
}

export async function getUserTagOverrides(): Promise<Tag[]> {
  const file = await fs.readFile(join(contentDir, "tags/user-tags.yaml"));
  const overrides = yaml.load(file.toString()) as {
    backgroundColor: string;
    color?: string;
    tags: { slug: string; name: string }[];
  }[];

  return overrides.flatMap(({ color, backgroundColor, tags }) =>
    tags.map(({ slug, name }) => ({
      slug,
      name,
      backgroundColor,
      color: color ?? getContrastingTextColor(backgroundColor),
    }))
  );
}
