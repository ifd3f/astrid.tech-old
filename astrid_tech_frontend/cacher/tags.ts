import fs from "fs-extra";
import path from "path";
import yaml from "js-yaml";
import { fillTagValues, Tag } from "../lib/db";
import { getContrastingTextColor } from "../lib/util";
import { Connection } from "typeorm";

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

export async function readLanguageTags(): Promise<Tag[]> {
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

    const tag = fillTagValues({
      title: key,
      shortName: getTagSlug(key),
      backgroundColor: lang.color,
    });

    out.push(tag);
  }

  return out;
}

export async function readUserTagFile(userTagFile: string): Promise<Tag[]> {
  const file = await fs.readFile(userTagFile);
  const overrides = yaml.load(file.toString()) as {
    backgroundColor: string;
    color?: string;
    tags: { slug: string; name: string }[];
  }[];

  return overrides.flatMap(({ color, backgroundColor, tags }) =>
    tags.map(({ slug, name }) => ({
      shortName: slug,
      title: name,
      backgroundColor,
      color: color ?? getContrastingTextColor(backgroundColor),
    }))
  );
}

export async function loadTags(
  conn: Connection,
  userTagsDir: string
): Promise<void> {
  console.log("Building language and user tag override tables");

  const ls: string[] = await fs.readdir(userTagsDir);

  const tasks = ls.map((subdir) =>
    readUserTagFile(path.join(userTagsDir, subdir))
  );
  tasks.push(readLanguageTags());

  const tags = (await Promise.all(tasks)).flat();

  await conn
    .createQueryBuilder()
    .insert()
    .into(Tag)
    .values(tags)
    .onConflict(`("shortName") DO NOTHING`)
    .execute();
}

/*
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
*/
