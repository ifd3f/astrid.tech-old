import moment from "moment"
import React, { FC, useEffect, useState } from "react"
import { Button, Col, Form, FormGroup, Input, Label } from "reactstrap"
import { CommentData } from "src/astrid-tech-api"
import { changeEventSetter, useCookieState } from "../../util/boilerplate"
import { useAPI } from "./APIProvider"

export type CommentingFormProps = {
  slug: string
  parent?: number
  onSuccessfullySubmitted?: () => {}
}

const COMMENT_EMAIL_COOKIE = "comment-email"
const COMMENT_NAME_COOKIE = "comment-name"
const COMMENT_WEBSITE_COOKIE = "comment-website"
const cookieOptions = {
  maxAge: 30 * 24 * 3600,
  path: "/",
  sameSite: "strict" as "strict",
}
const CommentingForm: FC<CommentingFormProps> = ({
  slug,
  parent,
  onSuccessfullySubmitted = () => {},
}) => {
  const [
    [name, setName],
    [email, setEmail],
    [website, setWebsite],
  ] = useCookieState(
    [COMMENT_EMAIL_COOKIE, COMMENT_NAME_COOKIE, COMMENT_WEBSITE_COOKIE],
    cookieOptions
  )

  const { api } = useAPI()
  const [isSubmitting, setIsSubmitting] = useState(false)
  const [body, setBody] = useState("")

  const submit = async () => {
    try {
      setIsSubmitting(true)
      await api.createComment(
        {
          author_name: name.length > 0 ? name : null,
          author_email: email,
          author_website: null,
          content_md: body,
          slug,
        },
        parent
      )
      setBody("")
      onSuccessfullySubmitted()
    } catch (e) {
      console.error(e)
    }

    setIsSubmitting(false)
  }

  return (
    <Form>
      <FormGroup row>
        <Label sm="4" for="comment-name">
          Name (leave blank for anonymous)
        </Label>
        <Col sm="8">
          <Input
            disabled={isSubmitting}
            onChange={changeEventSetter(setName)}
            type="text"
            value={name}
            name="name"
            id="comment-name"
            placeholder="Willy Shakespeare"
          />
        </Col>
      </FormGroup>

      <FormGroup row>
        <Label sm="4" for="comment-email">
          Email (required)
        </Label>
        <Col sm="8">
          <Input
            disabled={isSubmitting}
            onChange={changeEventSetter(setEmail)}
            type="email"
            value={email}
            name="email"
            id="comment-email"
            placeholder="user@example.com"
          />
        </Col>
      </FormGroup>

      <FormGroup row>
        <Label sm="4" for="comment-website">
          Website
        </Label>
        <Col sm="8">
          <Input
            disabled={isSubmitting}
            onChange={changeEventSetter(setWebsite)}
            type="url"
            value={website}
            name="website"
            id="comment-website"
            placeholder="my.cool.site"
          />
        </Col>
      </FormGroup>

      <FormGroup>
        <Label for="comment-body">Comment (markdown supported)</Label>
        <Input
          disabled={isSubmitting}
          onChange={ev => setBody(ev.target.value)}
          type="textarea"
          name="body"
          value={body}
          id="comment-body"
          placeholder="Type your comment here..."
        />
      </FormGroup>

      <Button color="primary" onClick={submit}>
        Submit!
      </Button>
    </Form>
  )
}

type CommentNodeProps = {
  comment: CommentData
}

const CommentNode: FC<CommentNodeProps> = ({ comment }) => {
  const date = moment(comment.time_authored)
  const name = comment.author_name ?? "[anonymous]"
  return (
    <div>
      <article>
        <p>
          <strong>{name}</strong> ({date.format("MMM DD YYYY")})
        </p>
        <div dangerouslySetInnerHTML={{ __html: comment.content_html }}></div>
      </article>
      <CommentList comments={comment.children} />
    </div>
  )
}

export type CommentListProps = {
  comments: CommentData[]
}

const CommentList: FC<CommentListProps> = ({ comments }) => {
  return (
    <>
      {comments.map(c => (
        <CommentNode key={c.id} comment={c} />
      ))}
    </>
  )
}

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
      <CommentingForm slug={slug} onSuccessfullySubmitted={fetchComments} />
      {comments ? (
        comments.length > 0 ? (
          <CommentList comments={comments} />
        ) : (
          <p>No comments. Start the conversation!</p>
        )
      ) : (
        <p>Loading comments...</p>
      )}
    </div>
  )
}
