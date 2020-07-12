import { CreateNodeArgs, GatsbyNode, Node } from "gatsby"
import { v4 } from "uuid"
import {
  TagNodeData,
  TAG_MIME_TYPE,
} from "../gatsby-astrid-plugin-tagging/index"
import { getContrastingTextColor, withContentDigest } from "../util"

type UserTagsYaml = {
  backgroundColor: string
  color?: string
  tags: {
    name: string
    slug: string
  }[]
}

export const onCreateNode: GatsbyNode["onCreateNode"] = async ({
  node,
  actions,
}: CreateNodeArgs) => {
  if (node.internal.type != "UserTagsYaml") return

  const { createNode, createParentChildLink } = actions

  const yamlNode = (node as unknown) as UserTagsYaml
  const color =
    yamlNode.color ?? getContrastingTextColor(yamlNode.backgroundColor)

  yamlNode.tags.forEach(tagSpecifier => {
    const { name, slug } = tagSpecifier

    const content: TagNodeData = {
      name,
      slug,
      color,
      backgroundColor: yamlNode.backgroundColor,
      priority: 1,
    }

    const tagNode = withContentDigest({
      id: v4(),
      internal: {
        type: "UserTag",
        mediaType: TAG_MIME_TYPE,
        content: JSON.stringify(content),
      },
      children: [],
    })

    createNode(tagNode)
    createParentChildLink({ parent: node, child: (tagNode as unknown) as Node })
  })
}
