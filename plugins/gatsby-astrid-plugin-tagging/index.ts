import { Node, NodeInput } from "gatsby"
import { v4 } from "uuid"
import { withContentDigest } from "../util"

export type Tag = {
  name: string
  slug: string
  color: string
  backgroundColor: string
  priority: number
}

export function buildTagNode(tag: Tag, parent?: string): NodeInput | Node {
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
