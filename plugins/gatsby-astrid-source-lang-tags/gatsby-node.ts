import axios from "axios"
import { GatsbyNode, Node, SourceNodesArgs } from "gatsby"
import yaml from "js-yaml"
import { getTextColor, buildTagNode, getTagSlug } from "../util"

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
    const textColor = getTextColor(lang.color)

    const node = buildTagNode({
      name: key,
      slug: getTagSlug(key),
      color: lang.color,
      textColor,
    }) as any
    // Create node with the gatsby createNode() API
    createNode(node)
  }
}
