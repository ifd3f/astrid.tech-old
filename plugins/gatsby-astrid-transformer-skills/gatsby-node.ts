import { GatsbyNode, Node } from "gatsby"
import { resolve } from "path"
import { v4 } from "uuid"
import { getTagId, withContentDigest } from "../util/index"

type YamlSkillNode = Node & {
  name: string
  skills: {
    slug: string
    level: number
  }[]
}
export const onCreateNode: GatsbyNode["onCreateNode"] = ({
  node,
  actions,
  getNode,
}) => {
  if (node.internal.type != "SkillsYaml") return

  const { createNode, createParentChildLink } = actions
  const yamlNode = (node as unknown) as YamlSkillNode

  const skillNode = withContentDigest({
    parent: yamlNode.id,
    internal: {
      type: "Skill",
    },
    children: [],
    id: v4(),

    name: yamlNode.name,
    skills: yamlNode.skills.map(({ slug, level }) => ({
      level: level,
      tag___NODE: getTagId(slug),
    })),
  })
  createNode(skillNode)
  createParentChildLink({ parent: yamlNode, child: skillNode })
}
