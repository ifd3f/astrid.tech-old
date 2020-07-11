import crypto from "crypto"
import { NodeInput, Node } from "gatsby"

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
