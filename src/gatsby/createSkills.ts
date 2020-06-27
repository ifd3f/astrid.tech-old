import { Actions, Node } from "gatsby"
import { v4 } from "uuid"
import { getTagId, withContentDigest } from "./util"

type YamlSkillNode = Node & {
  name: string
  skills: {
    slug: string
    level: number
  }[]
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

    name: yamlNode.name,
    skills: yamlNode.skills.map(({ slug, level }) => ({
      level: level,
      tag___NODE: getTagId(slug),
    })),
  })
  createNode(skillNode)
  createParentChildLink({ parent: yamlNode, child: skillNode })

  return skillNode
}
