import crypto from "crypto"
import { Node, NodeInput, NodePluginArgs, NodePluginSchema } from "gatsby"
import { FileSystemNode } from "gatsby-source-filesystem"
import path from "path"
import { v4 } from "uuid"

type TransientData = {
  id?: string
  parent?: string
  children?: string[]
}

type NodeData<T> = T & {
  internal: {
    type: string
  }
}

export type BlogMetadata = {
  title: string
  date: string
  description: string
  tags: string[]
}

export function buildNode<T>(
  nodeData: NodeData<T>,
  transient?: TransientData
): NodeData<T> & Node & NodeInput {
  const internal = {
    ...(nodeData.internal ?? {}),
    contentDigest: crypto
      .createHash(`md5`)
      .update(JSON.stringify(nodeData))
      .digest(`hex`),
  }
  return ({
    id: transient?.id ?? v4(),
    parent: transient?.parent,
    children: transient?.children,
    ...nodeData,
    internal,
  } as any) as NodeData<T> & Node & NodeInput
}

export type ResolveFileNodeArgs = {
  file: FileSystemNode
  relativePath: string
  getNodesByType: NodePluginArgs["getNodesByType"]
}

export function resolveFileNode({
  file,
  relativePath,
  getNodesByType,
}: ResolveFileNodeArgs): FileSystemNode | undefined {
  const absTargetPath = path.resolve(file.dir + "/" + relativePath)
  return (getNodesByType("File") as FileSystemNode[]).find(
    fileNode => fileNode.absolutePath == absTargetPath
  )
}

export function getContrastingTextColor(backgroundColor: string): string {
  const [, r, g, b] = backgroundColor
    .match(/#([0-9a-f]{2})([0-9a-f]{2})([0-9a-f]{2})/i)!
    .map(x => new Number("0x" + x) as number)
  return r * 0.299 + g * 0.587 + b * 0.114 > 140 ? "#000000" : "#ffffff"
}

export function getMarkdownStringType(
  pluginName: string,
  schema: NodePluginSchema
) {
  const name = `${pluginName}MarkdownString`
  return {
    name,
    type: schema.buildObjectType({
      name,
      fields: {
        id: "String!",
        text: "[String!]",
      },
      interfaces: ["Node"],
    }),
  }
}

export function buildMarkdownStringNode(pluginName: string, text: string) {
  const type = `${pluginName}MarkdownString`
  return buildNode({
    text,
    internal: {
      type,
      content: text,
      mediaType: "text/markdown",
    },
  })
}
