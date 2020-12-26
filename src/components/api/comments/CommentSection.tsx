import React, { FC, useEffect, useState } from "react"
import { CommentData } from "src/astrid-tech-api"
import { useAPI } from "../APIProvider"
import { CommentDataProvider } from "./CommentDataProvider"
import { CommentingForm } from "./CommentingForm"
import { CommentList } from "./CommentList"

export type CommentsSectionProps = {
  slug: string
}

export const CommentSection: FC<CommentsSectionProps> = ({ slug }) => {
  const { api } = useAPI()
  const [comments, setComments] = useState<CommentData[] | undefined>(undefined)

  const fetchComments = async () => {
    const response = await api.getComments(slug)
    setComments(response.data)
  }
  useEffect(() => {
    fetchComments()
  }, [slug])

  return (
    <div className="comments">
      <CommentDataProvider slug={slug}>
        <CommentingForm />
        {comments ? (
          comments.length > 0 ? (
            <CommentList comments={comments} />
          ) : (
            <p>No comments. Start the conversation!</p>
          )
        ) : (
          <p>Loading comments...</p>
        )}
      </CommentDataProvider>
    </div>
  )
}
