import React, { FC } from "react"
import { useAPI } from "../APIProvider"
import { CommentDataProvider, useCommentData } from "./CommentDataProvider"
import { CommentingForm } from "./CommentingForm"
import { CommentList } from "./CommentList"

export type CommentsSectionProps = {
  slug: string
}

const TopLevelCommentList = () => {
  const { state } = useCommentData()
  switch (state.status) {
    case "initial":
      return null
    case "success": {
      const comments = state.comments
      return comments.length > 0 ? (
        <CommentList comments={comments} />
      ) : (
        <p>No comments. Start the conversation!</p>
      )
    }
    case "fetching": {
      const comments = state.comments
      return (
        <div>
          <p>Fetching comments...</p>
          {comments ? <CommentList comments={comments} /> : null}
        </div>
      )
    }
    case "failure": {
      const comments = state.comments
      return (
        <div>
          <p>Failed to fetch comments.</p>
          {comments ? <CommentList comments={comments} /> : null}
        </div>
      )
    }
  }
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
