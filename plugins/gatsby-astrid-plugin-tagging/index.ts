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
