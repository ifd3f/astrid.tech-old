import { BlogPost } from "@astrid.tech/node-lib";
import { promises as fs } from "fs";
import matter from "gray-matter";
import path, { join, ParsedPath, relative } from "path";
import { walkArr } from "./util";

function getSlug(parsed: ParsedPath) {
  const tailname =
    parsed.name == "index" ? path.parse(parsed.dir).name : parsed.name;
  const oldStyleMatch = tailname.match(/\d{4}-\d{2}-\d{2}-(.+)/);
  if (oldStyleMatch) {
    return oldStyleMatch[1];
  }
  return tailname;
}

async function loadBlogPost(pathname: string) {
  const parsed = path.parse(pathname);
  const slug = getSlug(parsed);
  const fileContents = await fs.readFile(pathname);
  const { data, content } = matter(fileContents);
  const ordinal = data.ordinal ? data.ordinal : 0;

  return {
    tags: data.tags ? data.tags : [],
    assetRoot: path.parse(pathname).dir,
    slug,
    title: data.title,
    content: content,
    date: new Date(data.date),
    ordinal: ordinal,
    thumbnail: data.thumbnail ? join(parsed.dir, data.thumbnail) : null,
    description: data.description,
  };
}

export async function getBlogPosts(
  contentDir: string
): Promise<Promise<BlogPost<Date>>[]> {
  const searchRoot = join(contentDir, "blog");
  const files = (await walkArr(searchRoot))
    .filter(
      ({ stats }) =>
        stats.isFile() &&
        stats.name.endsWith(".md") &&
        !stats.name.endsWith(".note.md") &&
        !stats.name.endsWith(".recipe.md")
    )
    .map(({ root, stats }) =>
      loadBlogPost(join(root, stats.name)).then(
        (post) =>
          ({
            ...post,
            assetRoot: relative(searchRoot, root),
          } as BlogPost<Date>)
      )
    );
  return files;
}
