import { Actions, NodeInput } from "gatsby"
import { v4 } from "uuid"
import { buildTagNode, getTagId, withContentDigest } from "./util"

type YamlClassNode = {
  name: string
  number: string
  tags: string[]
  desc?: string
}

type ClassNodeArg = NodeInput & {
  name: string
  number: string
  tags___NODE: string[]
  desc: string | null
}

type YamlEducationNode = NodeInput & {
  name: string
  slug: string
  degree?: string
  classes: YamlClassNode[]
}

type EducationNodeArg = NodeInput & {}

export function createCourseTagNode(actions: Actions, courseNode: any) {
  const { createNode, createParentChildLink } = actions
  const tagNode = buildTagNode({
    parent: courseNode.id,
    name: courseNode.number,
    slug: courseNode.slug,
    color: "#18b21b",
    textColor: "#ffffff",
  })
  console.log(tagNode.children)
  createNode(tagNode)
  createParentChildLink(courseNode, tagNode as any)

  return tagNode
}

function createCourseNode(
  actions: Actions,
  parentSlug: string,
  parentId: string,
  yamlNode: any
) {
  const { createNode } = actions

  const slug =
    parentSlug + yamlNode.number.replace(" ", "-").toLowerCase() + "/"

  const courseNode = withContentDigest({
    parent: parentId,
    internal: {
      type: "Course",
    },
    id: v4(),
    children: [],

    name: yamlNode.name,
    slug: slug,
    number: yamlNode.number,
    desc: yamlNode.desc ? yamlNode.desc : null,
    tags___NODE: yamlNode.tags.map(getTagId),
  })

  createNode(courseNode)

  return courseNode
}

export function createEducationNode(actions: Actions, yamlNode: any) {
  const { createNode } = actions

  const slug = "/education/" + yamlNode.slug + "/"

  const courseNodes = yamlNode.courses.map((raw: any) =>
    createCourseNode(actions, slug, yamlNode.id, raw)
  )

  const educationNode = withContentDigest({
    parent: yamlNode.id,
    internal: {
      type: "Education",
    },
    children: [],
    id: v4(),

    name: yamlNode.name,
    degree: yamlNode.degree,
    slug: slug,
    courses___NODE: courseNodes.map((classNode: any) => classNode.id),
  })
  createNode(educationNode)

  return educationNode
}
