import { promises as fs } from "fs";
import yaml from "js-yaml";
import { join } from "path";
import { getContrastingTextColor } from "@astrid.tech/node-lib";
import { Tag } from "@astrid.tech/node-lib";

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
