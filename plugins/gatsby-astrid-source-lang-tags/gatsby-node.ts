import axios from "axios"
import { GatsbyNode, Node, SourceNodesArgs } from "gatsby"
import yaml from "js-yaml"
import { getTextColor, getTagSlug, withContentDigest } from "../util"
import { buildTagNode } from "../gatsby-astrid-plugin-tagging"

type LinguistEntry = {
  color: string
}

type LinguistData = {
  [name: string]: LinguistEntry
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
    const color = getTextColor(lang.color)

    createNode(
      buildTagNode({
        name: key,
        slug: getTagSlug(key),
        color,
        backgroundColor: lang.color,
        priority: 1,
      })
    )
  }
}
