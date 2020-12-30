import { promises as fs } from "fs";
import yaml from "js-yaml";
import { Tag } from "../types/types";
import { getContrastingTextColor } from "./util";

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
