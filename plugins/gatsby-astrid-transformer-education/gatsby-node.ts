import { Actions, GatsbyNode, Node, SourceNodesArgs } from "gatsby"
import { v4 } from "uuid"
import { withContentDigest } from "../util"

type CourseYamlData = Node & {
  name: string
  number: string
  desc: string
  date: string
  tags: string[]
}

type SchoolYamlNode = Node & {
  slug: string
  name: string
  degree: string
  startDate: string
  endDate: string
  courses: CourseYamlData[]
}

// function createCourseTagNode(actions: Actions, courseNode: any) {
//   const { createNode, createParentChildLink } = actions
//   const tagNode = buildTagNode({
//     parent: courseNode.id,
//     name: courseNode.number,
//     slug: courseNode.slug,
//     color: "#18b21b",
//     textColor: "#ffffff",
//   })
//   console.log(tagNode.children)
//   createNode(tagNode)
//   createParentChildLink({ parent: courseNode, child: tagNode as any })

//   return tagNode
// }

type CreateCourseNodeArgs = {
  actions: Actions
  schoolSlug: string
  schoolYamlNode: SchoolYamlNode
  courseYamlData: CourseYamlData
}

function createCourseNode({
  actions,
  schoolSlug,
  schoolYamlNode,
  courseYamlData,
}: CreateCourseNodeArgs) {
  const { createNode, createParentChildLink } = actions

  const slug =
    schoolSlug + courseYamlData.number.replace(" ", "-").toLowerCase() + "/"

  const courseNode = withContentDigest({
    parent: schoolYamlNode.id,
    internal: {
      type: "Course",
    },
    id: v4(),
    children: [],

    name: courseYamlData.name,
    slug: slug,
    number: courseYamlData.number,
    date: courseYamlData.date,
    description: courseYamlData.description,
    tagSlugs: courseYamlData.tags,
  })

  createNode(courseNode)
  createParentChildLink({
    parent: schoolYamlNode,
    child: (courseNode as unknown) as Node,
  })

  return courseNode
}

type CreateSchoolNodeArgs = {
  actions: Actions
  yamlNode: SchoolYamlNode
}

function createSchoolNode({ actions, yamlNode }: CreateSchoolNodeArgs) {
  const { createNode } = actions

  const slug = "/education/" + yamlNode.slug + "/"

  const courseNodes = yamlNode.courses.map(courseYamlData =>
    createCourseNode({
      actions,
      schoolYamlNode: yamlNode,
      schoolSlug: slug,
      courseYamlData,
    })
  )

  const schoolNode = withContentDigest({
    parent: yamlNode.id,
    internal: {
      type: "School",
    },
    children: [],
    id: v4(),

    name: yamlNode.name,
    degree: yamlNode.degree,
    slug: slug,
    startDate: yamlNode.startDate,
    endDate: yamlNode.endDate,
    courses___NODE: courseNodes.map((classNode: any) => classNode.id),
  })
  createNode(schoolNode)

  return schoolNode
}

export const sourceNodes: GatsbyNode["sourceNodes"] = async ({
  actions,
  schema,
}: SourceNodesArgs) => {
  const { createTypes } = actions

  const Course = schema.buildObjectType({
    name: "Course",
    fields: {
      name: "String!",
      date: "Date!",
      slug: "String!",
      number: "String!",
      description: "String",
      tags: { type: "Tag!", extensions: { tagify: {} } },
      tagSlugs: "String!",
    },
    interfaces: ["Node"],
  })

  const School = schema.buildObjectType({
    name: "School",
    fields: {
      name: "String!",
      degree: "String!",
      slug: "String!",
      startDate: "Date!",
      endDate: "Date!",
      courses: "[Course!]",
    },
    interfaces: ["Node"],
  })

  createTypes([Course, School])
}

export const onCreateNode: GatsbyNode["onCreateNode"] = async ({
  node,
  actions,
}) => {
  if (node.internal.type != "SchoolYaml") return
  const yamlNode = (node as unknown) as SchoolYamlNode

  createSchoolNode({ actions, yamlNode })
}
