import { Actions, Node } from "gatsby"
import { Notebook } from "../types/nbformat-v4"
import { withContentDigest, createLinkedTagList } from "./util"
import { v4 } from "uuid"
import path from "path"

type JupyterNotebookNode = Node & {
  json: Notebook
  internal: {
    content: string
  }
  fileAbsolutePath: string
  data: {
    metadata: {
      blog_data: BlogMetadata
    }
  }
}

type BlogMetadata = {
  title: string
  date: string
  description: string
  tags: string[]
}

export function createJupyterBlogPostNode(
  actions: Actions,
  jupyterNode: JupyterNotebookNode
) {
  const { createNode, createParentChildLink } = actions
  const { title, date, description, tags } = jupyterNode.data.metadata.blog_data

  const slugBase = path.parse(path.dirname(jupyterNode.fileAbsolutePath)).name

  const postNode = withContentDigest({
    parent: jupyterNode.id,
    internal: {
      type: "BlogPost",
    },
    id: v4(),
    children: [],
    slug: `/blog/${slugBase}/`,
    title,
    date,
    description,

    contentType: "jupyter",
    markdown___NODE: null,
    mdx___NODE: null,
    jupyter___NODE: jupyterNode.id,

    tags: createLinkedTagList(tags),
  })

  createNode(postNode)
  createParentChildLink({ parent: jupyterNode, child: postNode })
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
