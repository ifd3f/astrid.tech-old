import { promises as fs } from "fs";
import matter from "gray-matter";
import path, { join, ParsedPath, relative } from "path";
import { BlogPost } from "../types/types";
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

  return {
    tags: data.tags,
    assetRoot: path.parse(pathname).dir,
    slug,
    title: data.title,
    content: content,
    date: new Date(data.date),
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

async function buildBlogPostCache(db: Database) {
  console.log("Building blog post cache");

  const insertPost = db.prepare(
    `INSERT INTO blog_post(
      title, 
      asset_root, 
      slug, 
      date, 
      year, 
      month, 
      day, 
      ordinal,
      description, 
      content) 
    VALUES (@title, 
      @assetRoot, 
      @slug, 
      @date, 
      @year, 
      @month, 
      @day, 
      @ordinal,
      @description, 
      @content)`
  );
  const blogPosts = await Promise.all(await getBlogPosts(contentDir));
  const ids = db
    .transaction(() =>
      blogPosts.map((post) =>
        insertPost.run({
          ...post,
          date: post.date.toISOString(),
          year: post.date.getUTCFullYear(),
          month: post.date.getUTCMonth() + 1,
          day: post.date.getUTCDate(),
          ordinal: 0,
        })
      )
    )()
    .map((result, i) => ({
      id: result.lastInsertRowid,
      post: blogPosts[i],
    }));

  const insertTag = db.prepare(
    "INSERT INTO blog_tag (fk_blog, tag) VALUES (@postId, @tag)"
  );

  db.transaction(() => {
    ids.map(({ id, post }) =>
      post.tags.map((tag) => insertTag.run({ tag, postId: id }))
    );
  })();
}
