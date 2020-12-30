import { promises as fs } from "fs";
import matter from "gray-matter";
import path, { join, ParsedPath } from "path";
import { BlogPost } from "../types/types";
import { walkArr } from "./util";

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

export async function getBlogPosts(
  contentDir: string
): Promise<Promise<BlogPostWithDir<Date>>[]> {
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
