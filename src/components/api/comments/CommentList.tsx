import React, { FC } from "react"
import { WrappedComment } from "./CommentFetching"
import { CommentNode } from "./CommentNode"

export type CommentListProps = {
  comments: WrappedComment[]
  isReply?: boolean
}

export const CommentList: FC<CommentListProps> = ({
  comments,
  isReply = false,
}) => {
  return (
    <>
      {comments.map((c, i) => {
        return <CommentNode key={c.id} comment={c} isReply={isReply} />
      })}
    </>
  )
}
