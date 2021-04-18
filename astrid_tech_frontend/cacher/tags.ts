import { promises as fs } from "fs";
import yaml from "js-yaml";
import { join } from "path";
import { getContrastingTextColor } from "../lib/util";
import { Tag } from "../types/types";

type LinguistEntry = {
  color: string;
};

type LinguistData = {
  [name: string]: LinguistEntry;
};

const SLUG_OVERRIDE = new Map<string, string>([
  ["c++", "cpp"],
  ["c#", "csharp"],
  ["f#", "fsharp"],
  ["objective-c++", "objective-cpp"],
]);

function getTagSlug(name: string): string {
  const lower = name.toLowerCase();
  return SLUG_OVERRIDE.get(lower) || lower.replace(" ", "-");
}

export async function getLanguageTags() {
  const langs = yaml.load(
    await fs.readFile(`${__dirname}/languages.yml`, {
      encoding: "utf-8",
      flag: "r",
    })
  ) as LinguistData;

  const out: Tag[] = [];

  for (let key in langs) {
    if (!langs.hasOwnProperty(key)) {
      continue;
    }

    const lang = langs[key];
    if (!lang.color) {
      continue;
    }
    const color = getContrastingTextColor(lang.color);
    out.push({
      name: key,
      slug: getTagSlug(key),
      color,
      backgroundColor: lang.color,
    });
  }

  return out;
}

export async function getUserTagOverrides(contentDir: string): Promise<Tag[]> {
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
