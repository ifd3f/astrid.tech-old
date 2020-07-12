import { Node, NodeInput } from "gatsby"
import { v4 } from "uuid"
import { withContentDigest } from "../util"

export const TAG_MIME_TYPE = "application/prs.astrid-tech-tag"

export type TagContent = {
  name: string
  slug: string
  color: string
  backgroundColor: string
}

export type TagNodeData = TagContent & {
  priority: number
}

export function buildTagNode(
  tag: TagNodeData,
  parent?: string
): NodeInput | Node {
  return withContentDigest({
    parent,
    internal: {
      type: `Tag`,
    } as any,
    children: [],

    id: v4(),
    name: tag.name,
    slug: tag.slug,
    color: tag.color,
    priority: tag.priority,
    backgroundColor: tag.backgroundColor,
  })
}
