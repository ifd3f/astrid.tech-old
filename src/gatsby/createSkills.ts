import { Actions, Node } from "gatsby"
import { v4 } from "uuid"
import { getTagId, withContentDigest } from "./util"

type YamlSkillNode = Node & {
  slug: string
  level: number
}

export function createSkillNode(actions: Actions, yamlNode: YamlSkillNode) {
  const { createNode, createParentChildLink } = actions

  const skillNode = withContentDigest({
    parent: yamlNode.id,
    internal: {
      type: "Skill",
    },
    children: [],
    id: v4(),

    level: yamlNode.level,
    tag___NODE: getTagId(yamlNode.slug),
  })
  createNode(skillNode)
  createParentChildLink({ parent: yamlNode, child: skillNode })

  return skillNode
}
