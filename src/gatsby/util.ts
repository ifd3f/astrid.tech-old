import crypto from "crypto"
import { Tag } from "../types/index"
import { NodeInput } from "gatsby"

const SLUG_OVERRIDE = new Map<string, string>([
  ["c++", "cpp"],
  ["c#", "csharp"],
  ["f#", "fsharp"],
  ["objective-c++", "objective-cpp"],
])

export function getTagSlug(name: string): string {
  const lower = name.toLowerCase()
  return SLUG_OVERRIDE.get(lower) || lower.replace(" ", "-")
}

export function getTagId(slug: string): string {
  return crypto
    .createHash("md5")
    .update(`astrid.tech-tag-${slug}`)
    .digest("hex")
}

type PreContentDigestNode = {
  internal: any
}

export function withContentDigest<T extends PreContentDigestNode>(
  node: T
): NodeInput {
  // Get content digest of node. (Required field)
  const contentDigest = crypto
    .createHash(`md5`)
    .update(JSON.stringify(node))
    .digest(`hex`)
  // add it to userNode
  node.internal.contentDigest = contentDigest
  return (node as unknown) as NodeInput
}

type TagNode = {
  slug: string
  tag___NODE: string
}

export function createLinkedTagList(slugs: string[]): TagNode[] {
  return slugs.map(slug => ({
    slug: slug,
    tag___NODE: getTagId(slug),
  }))
}

type BuildTagNodeArgs = {
  name: string
  slug: string
  color: string
  textColor: string
  parent?: string
}

export function buildTagNode({
  name,
  slug,
  color,
  textColor,
  parent = "__SOURCE__",
}: BuildTagNodeArgs): NodeInput {
  return withContentDigest({
    parent,
    internal: {
      type: `Tag`,
    },
    children: [],

    id: getTagId(slug),
    name,
    slug,
    color,
    textColor,
  })
}
