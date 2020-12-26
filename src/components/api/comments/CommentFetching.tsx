// This is to force re-renders of changing children.
import { CommentData } from "src/astrid-tech-api"

export type WrappedComment = {
  data: CommentData
  totalChildren: number
  wrappedChildren: WrappedComment[]
}

export function wrap(comment: CommentData): WrappedComment {
  const wrappedChildren = comment.children.map(wrap)
  const totalChildren =
    wrappedChildren.reduce(
      (count, comment) => count + comment.totalChildren,
      0
    ) + wrappedChildren.length

  return {
    data: comment,
    totalChildren,
    wrappedChildren,
  }
}
