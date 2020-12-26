import { useLocation } from "@reach/router"
import moment from "moment"
import React, { FC, ReactNode, useState } from "react"
import { FaFlag, FaLink, FaReply } from "react-icons/fa"
import { Button } from "reactstrap"
import { CommentData } from "src/astrid-tech-api"
import { useCommentData } from "./CommentDataProvider"
import { CommentingForm } from "./CommentingForm"
import { CommentList } from "./CommentList"
import { ReportingForm } from "./ReportingForm"

type CommentNodeProps = {
  comment: CommentData
  isReply?: boolean
}

type CommentState = "reply" | "report" | null

export const CommentNode: FC<CommentNodeProps> = ({
  comment,
  isReply = false,
}) => {
  const { refreshComments } = useCommentData()
  const date = moment(comment.time_authored)
  const name = comment.author_name ?? "[anonymous]"
  const user = (
    <>
      <strong>{name}</strong>
      {comment.author_email ? `<${comment.author_email}>` : null}
    </>
  )
  const location = useLocation()
  const commentId = `comment-${comment.id}`
  const url = new URL(location.href)
  url.hash = commentId

  const [actionState, setActionState] = useState<CommentState>(null)

  const toggleAction = (newState: CommentState) => () => {
    setActionState(actionState == newState ? null : newState)
  }

  const onSubmitted = () => {
    setActionState(null)
    refreshComments()
  }

  let form: ReactNode
  switch (actionState) {
    case null:
      form = null
      break
    case "reply":
      form = <CommentingForm replyTo={comment.id} onSubmitted={onSubmitted} />
      break
    case "report":
      form = <ReportingForm comment={comment.id} onSubmitted={onSubmitted} />
      break
  }

  return (
    <div>
      <hr />
      <article className="comment" id={commentId}>
        <p className="d-flex">
          <div className="header mr-auto">
            {comment.author_website ? (
              <a href={comment.author_website}>{user}</a>
            ) : (
              user
            )}{" "}
            <span className="text-muted">
              {isReply ? "replied " : null}
              on {date.format("MMM DD, YYYY HH:mm:ss")}
            </span>
          </div>
          <div className="">
            <Button
              outline={actionState != "reply"}
              onClick={toggleAction("reply")}
              color="primary"
              size="sm"
              style={{ fontSize: 12 }}
            >
              <FaReply title="reply" /> Reply
            </Button>{" "}
            <Button
              outline={actionState != "report"}
              onClick={toggleAction("report")}
              color="danger"
              size="sm"
              style={{ fontSize: 12 }}
            >
              <FaFlag title="flag" /> Report
            </Button>{" "}
            <Button
              href={url.href}
              outline
              color="info"
              size="sm"
              style={{ fontSize: 12 }}
            >
              <FaLink title="link" /> Permalink
            </Button>
          </div>
        </p>
        <div
          className="body"
          dangerouslySetInnerHTML={{ __html: comment.content_html }}
        />
        <div className="children">
          {form}
          <CommentList comments={comment.children} isReply />
        </div>
      </article>
    </div>
  )
}
