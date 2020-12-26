import React, { FC } from "react"
import { CommentData } from "src/astrid-tech-api"
import { CommentNode } from "./CommentNode"

export type CommentListProps = {
  comments: CommentData[]
  onSubmission?: () => void
}

export const CommentList: FC<CommentListProps> = ({ comments }) => {
  return (
    <>
      {comments.map(c => (
        <CommentNode key={c.id} comment={c} />
      ))}
    </>
  )
}
