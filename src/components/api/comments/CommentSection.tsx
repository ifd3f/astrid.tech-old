import React, { FC } from "react"
import { useAPI } from "../APIProvider"
import { CommentDataProvider, useCommentData } from "./CommentDataProvider"
import { wrap } from "./CommentFetching"
import { CommentingForm } from "./CommentingForm"
import { CommentList } from "./CommentList"

export type CommentsSectionProps = {
  slug: string
}

const TopLevelCommentList = () => {
  const { comments } = useCommentData()
  return comments != null ? (
    comments.length > 0 ? (
      <CommentList comments={comments.map(wrap)} />
    ) : (
      <p>No comments. Start the conversation!</p>
    )
  ) : (
    <p>Loading comments...</p>
  )
}

export const CommentSection: FC<CommentsSectionProps> = ({ slug }) => {
  const { api } = useAPI()

  return (
    <div className="comments">
      <CommentDataProvider slug={slug}>
        <CommentingForm />
        <TopLevelCommentList />
      </CommentDataProvider>
    </div>
  )
}
