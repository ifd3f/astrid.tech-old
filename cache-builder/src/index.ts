import {
  getContrastingTextColor,
  getHSLString,
  getPersistentColor,
  RichColorTheme,
  convertProjectToStringDate,
 Tag,
} from "@astrid.tech/node-lib";
import sqlite3, { Database } from "better-sqlite3";
import { promises as fs } from "fs";
import path from "path";
import { copyAssets, mapData } from "./assets";
import { getBlogPosts } from "./blog";
import { generateLicenses } from "./licenses";
import { getProjects } from "./projects";
import { buildRSSFeed } from "./rss";
import { getLanguageTags, getUserTagOverrides } from "./tags";
import { buildTWTXT } from "./twtxt";
import { serializeJS } from "./util";

const contentDir = path.join(process.cwd(), "../content");
const TAGS_D_TS = `import { Tag } from "../lib/cache";
declare const tags: Tag[];
export default tags;`;

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
      blogPosts.map((post) => {
        return insertPost.run({
          ...post,
          date: post.date.toISOString(),
          year: post.date.getUTCFullYear(),
          month: post.date.getUTCMonth() + 1,
          day: post.date.getUTCDate(),
          ordinal: post.ordinal,
        });
      })
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
      post.tags.map((tag: any) => insertTag.run({ tag, postId: id }))
    );
  })();
}

function insertTags(db: Database, tags: Tag[]) {
  const insert = db.prepare(
    "INSERT INTO tag (slug, name, color, background_color) VALUES (@slug, @name, @color, @backgroundColor)"
  );

  db.transaction(() => {
    for (const tag of tags) {
      insert.run(tag);
    }
  })();
}

async function buildTagOverrideTable(db: Database) {
  console.log("Building language and user tag override tables");

  const [langTags, userTags] = await Promise.all([
    getLanguageTags(),
    getUserTagOverrides(contentDir),
  ]);

  insertTags(db, langTags);
  insertTags(db, userTags);
}

async function buildProjectTagOverrideTable(db: Database) {
  console.log("Building project tag override tables");

  const projects = db.prepare("SELECT slug, title FROM project").all();

  insertTags(
    db,
    projects.map(({ slug, title }) => {
      const backgroundColor = getHSLString(
        getPersistentColor(slug, RichColorTheme)
      );
      return {
        slug: `/projects/${slug}/`,
        name: title,
        backgroundColor,
        color: getContrastingTextColor(backgroundColor),
      };
    })
  );
}

async function exportProjectData(db: Database, dest: string) {}

async function exportTagOverrideData(db: Database, dest: string) {
  console.log("Exporting tag overrides to file");
  const tags = db
    .prepare(
      `SELECT t1.slug, IFNULL(t2.c, 0) as count, name, color, background_color AS backgroundColor FROM tag AS t1
      LEFT JOIN (
        SELECT tag as slug, SUM(count) AS c FROM (
          SELECT tag, COUNT(fk_project) AS count FROM project_tag GROUP BY tag UNION ALL
          SELECT tag, COUNT(fk_blog) AS count FROM blog_tag GROUP BY tag
        )
        GROUP BY tag
      ) AS t2
      ON t1.slug = t2.slug`
    )
    .all()
    .map((tag) => {
      const backgroundColor =
        tag.backgroundColor ?? getHSLString(getPersistentColor(tag.slug));
      return {
        slug: tag.slug,
        name: tag.name ?? tag.slug,
        backgroundColor,
        count: tag.count,
        color: tag.color ?? getContrastingTextColor(backgroundColor),
      } as Tag;
    });

  await fs.writeFile(
    path.join(process.cwd(), "data/tags.js"),
    serializeJS(tags)
  );

  await fs.writeFile(path.join(process.cwd(), "data/tags.d.ts"), TAGS_D_TS);
}

async function buildProjectCache(db: Database) {
  console.log("Building project cache");

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
      @description,
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
    projects.map((project) =>
      insertProject.run({
        ...convertProjectToStringDate(project),
        sourceUrls: JSON.stringify(project.source),
      })
    )
  )();

  const insertTag = db.prepare(
    "INSERT INTO project_tag (fk_project, tag) VALUES (@projectId, @tag)"
  );

  db.transaction(() => {
    projects.forEach((project, i) =>
      project.tags.map((tag: any) =>
        insertTag.run({ projectId: results[i].lastInsertRowid, tag })
      )
    );
  })();
}

async function main(dbUrl: string) {
  await fs.rm(dbUrl, { force: true });

  const dataDir = path.join(__dirname, "../data");
  const db = sqlite3(dbUrl, {});
  const initSchema = (
    await fs.readFile(path.join(__dirname, "schema.sql"))
  ).toString();
  db.exec(initSchema);

  await copyAssets(contentDir, path.join(__dirname, "../public/_/"));
  await mapData(contentDir, path.join(dataDir, "objs"));

  await Promise.all([
    buildBlogPostCache(db),
    buildProjectCache(db),
    buildTagOverrideTable(db),
    buildProjectTagOverrideTable(db),
  ]);

  await Promise.all([
    exportTagOverrideData(db, path.join(dataDir, "tags.js")),
    generateLicenses(path.join(dataDir, "licenses.json")),
    buildRSSFeed(path.join(__dirname, "../public")),
    buildTWTXT(path.join(__dirname, "../public")),
  ]);
}

main("content.sqlite3").catch((e) => {
  throw e;
});
