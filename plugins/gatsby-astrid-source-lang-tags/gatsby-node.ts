import axios from "axios"
import { GatsbyNode, Node, SourceNodesArgs } from "gatsby"
import yaml from "js-yaml"
import { withContentDigest, getContrastingTextColor } from "../util"
import { v4 } from "uuid"
import { TAG_MIME_TYPE } from "../gatsby-astrid-plugin-tagging/index"

type LinguistEntry = {
  color: string
}

type LinguistData = {
  [name: string]: LinguistEntry
}

const SLUG_OVERRIDE = new Map<string, string>([
  ["c++", "cpp"],
  ["c#", "csharp"],
  ["f#", "fsharp"],
  ["objective-c++", "objective-cpp"],
])

function getTagSlug(name: string): string {
  const lower = name.toLowerCase()
  return SLUG_OVERRIDE.get(lower) || lower.replace(" ", "-")
}

export const sourceNodes: GatsbyNode["sourceNodes"] = async ({
  actions,
}: SourceNodesArgs) => {
  const { createNode } = actions

  const res = await axios.get(
    "https://raw.githubusercontent.com/github/linguist/master/lib/linguist/languages.yml"
  )

  const langs = yaml.load(res.data) as LinguistData

  for (let key in langs) {
    if (!langs.hasOwnProperty(key)) {
      continue
    }

    const lang = langs[key]
    if (!lang.color) {
      continue
    }
    const color = getContrastingTextColor(lang.color)

    createNode(
      withContentDigest({
        id: v4(),
        internal: {
          type: "LinguistLanguage",
          mediaType: TAG_MIME_TYPE,
          content: JSON.stringify({
            name: key,
            slug: getTagSlug(key),
            color,
            backgroundColor: lang.color,
          }),
        },
        children: [],
        data: langs,
      })
    )
  }
}
