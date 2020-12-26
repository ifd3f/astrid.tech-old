import React, { FC } from "react"
import { CommentData } from "src/astrid-tech-api"
import { CommentNode } from "./CommentNode"

export type CommentListProps = {
  comments: CommentData[]
  isReply?: boolean
}

export const CommentList: FC<CommentListProps> = ({
  comments,
  isReply = false,
}) => {
  return (
    <>
      {comments.map(c => (
        <CommentNode comment={c} isReply={isReply} />
      ))}
    </>
  )
}
