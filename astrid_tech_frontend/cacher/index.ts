import sqlite3, { Database } from "better-sqlite3";
import { promises as fs } from "fs";
import path from "path";
import { BlogPostWithDir, getBlogPosts, getUserTagOverrides } from "./content";
import { getLanguageTags } from "./languageTags";

async function buildBlogPostCache(db: Database) {
  const insertPost = db.prepare(
    "INSERT INTO blog_post(title, asset_root, slug, date, description, content) VALUES (@title, @assetRoot, @slug, @date, @description, @content)"
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

async function buildTagOverrideTable(db: Database) {
  const [langTags, userTags] = await Promise.all([
    getLanguageTags(),
    getUserTagOverrides(),
  ]);
  const insert = db.prepare(
    "INSERT INTO tag (slug, name, color, background_color) VALUES (@slug, @name, @color, @backgroundColor)"
  );
  for (const tag of langTags) {
    insert.run(tag);
  }
  for (const tag of userTags) {
    insert.run(tag);
  }
}

async function main(dbUrl: string) {
  await fs.rm(dbUrl);

  const db = sqlite3(dbUrl, {});
  const initSchema = (
    await fs.readFile(path.join(__dirname, "schema.sql"))
  ).toString();
  db.exec(initSchema);

  await buildBlogPostCache(db);
  await buildTagOverrideTable(db);
}

main("content.sqlite3");
