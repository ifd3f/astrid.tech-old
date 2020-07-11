import crypto from "crypto"
import { NodeInput, Node } from "gatsby"

const SLUG_OVERRIDE = new Map<string, string>([
  ["c++", "cpp"],
  ["c#", "csharp"],
  ["f#", "fsharp"],
  ["objective-c++", "objective-cpp"],
])

export function getTextColor(backgroundColor: string): string {
  const [, r, g, b] = backgroundColor
    .match(/#([0-9a-f]{2})([0-9a-f]{2})([0-9a-f]{2})/i)!
    .map(x => new Number("0x" + x) as number)
  return r * 0.299 + g * 0.587 + b * 0.114 > 186 ? "#000000" : "#ffffff"
}

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
