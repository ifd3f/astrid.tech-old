// This is to force re-renders of changing children.
import { CommentData } from "src/astrid-tech-api"

export type WrappedComment = {
  data: CommentData
  id: string
  wrappedChildren: WrappedComment[]
}

export function wrap(comment: CommentData): WrappedComment {
  const wrappedChildren = comment.children.map(wrap)
  const id = wrappedChildren.map(c => c.id)
  id.push(comment.id.toString())

  return {
    data: comment,
    id: id.join(" "),
    wrappedChildren,
  }
}
