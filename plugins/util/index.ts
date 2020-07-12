import crypto from "crypto"
import { NodeInput, Node, NodePluginArgs } from "gatsby"
import { FileSystemNode } from "gatsby-source-filesystem"
import path from "path"

type PreContentDigestNode = {
  id: string
  parent?: string
  children?: string[]
  internal: {
    type: string
    mediaType?: string
    content?: string
    description?: string
    contentDigest?: string
  }
  [key: string]: unknown
}

export function withContentDigest<T extends PreContentDigestNode>(
  node: T
): T & NodeInput {
  // Get content digest of node. (Required field)
  const contentDigest = crypto
    .createHash(`md5`)
    .update(JSON.stringify(node))
    .digest(`hex`)
  // add it to userNode
  node.internal.contentDigest = contentDigest
  return (node as unknown) as T & NodeInput
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
