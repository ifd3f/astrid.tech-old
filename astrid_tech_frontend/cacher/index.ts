import sqlite3 from "better-sqlite3";
import { promises as fs } from "fs";
import path from "path";
import { BlogPostWithDir, getBlogPosts } from "./content";

async function main(dbUrl: string) {
  await fs.rm(dbUrl);

  const db = sqlite3(dbUrl, {});
  const initSchema = (
    await fs.readFile(path.join(__dirname, "schema.sql"))
  ).toString();
  db.exec(initSchema);

  const insertPost = db.prepare(
    "INSERT INTO blog_post(title, asset_root, slug, date, content) VALUES (@title, @assetRoot, @slug, @date, @content)"
  );
  const insertManyPosts = db.transaction((data: BlogPostWithDir<Date>[]) =>
    data.map(({ assetRoot, post }) =>
      insertPost.run({
        assetRoot,
        ...post,
        date: post.date.toISOString(),
      })
    )
  );

  const blogPosts = await Promise.all(await getBlogPosts());
  const ids = insertManyPosts(blogPosts).map((result, i) => ({
    id: result.lastInsertRowid,
    post: blogPosts[i].post,
  }));

  const insertTag = db.prepare(
    "INSERT INTO blog_tag (fk_blog, tag) VALUES (@postId, @tag)"
  );

  const insertManyTag = db.transaction((data: typeof ids) => {
    data.map(({ id, post }) =>
      post.tags.map((tag) => insertTag.run({ tag, postId: id }))
    );
  });

  insertManyTag(ids);
}

main("content.sqlite3");
