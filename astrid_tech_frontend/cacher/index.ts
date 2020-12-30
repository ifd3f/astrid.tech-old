import sqlite3, { Database } from "better-sqlite3";
import { promises as fs } from "fs";
import path from "path";
import { convertProjectToStringDate } from "../types/types";
import { BlogPostWithDir, getBlogPosts } from "./blog";
import { getProjects } from "./projects";
import { getLanguageTags, getUserTagOverrides } from "./tags";

const contentDir = path.join(process.cwd(), "content");

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

  const blogPosts = await Promise.all(await getBlogPosts(contentDir));
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
    getUserTagOverrides(contentDir),
  ]);
  const tags = langTags.concat(userTags);
  const insert = db.prepare(
    "INSERT INTO tag (slug, name, color, background_color) VALUES (@slug, @name, @color, @backgroundColor)"
  );
  for (const tag of tags) {
    insert.run(tag);
  }

  await fs.writeFile(
    path.join(process.cwd(), "data/tags.js"),
    "/* This is an AUTO-GENERATED FILE. */\nmodule.exports=" +
      JSON.stringify(tags)
  );
}

async function buildProjectCache(db: Database) {
  const insertProject = db.prepare(
    `INSERT INTO project(
      asset_root, 
      title, 
      description,
      slug, 
      start_date, 
      end_date,
      url,
      status,
      source_urls,
      thumbnail_path, 
      content) 
    VALUES (
      @assetRoot,
      @title, 
      @description
      @slug, 
      @startDate, 
      @endDate, 
      @url,
      @status,
      @sourceUrls,
      @thumbnail,
      @content)`
  );
  const projects = await Promise.all(await getProjects(contentDir));

  const results = db.transaction(() =>
    projects.map(({ assetRoot, object }) =>
      insertProject.run({
        assetRoot,
        ...convertProjectToStringDate(object),
        sourceUrls: JSON.stringify(object.source),
      })
    )
  )();

  const insertTag = db.prepare(
    "INSERT INTO project_tag (fk_project, tag) VALUES (@projectId, @tag)"
  );

  db.transaction(() => {
    projects.forEach(({ object }, i) =>
      object.tags.map((tag) =>
        insertTag.run({ projectId: results[i].lastInsertRowid, tag })
      )
    );
  })();
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
