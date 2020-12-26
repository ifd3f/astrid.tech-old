import moment from "moment"
import React, { FC, useEffect, useState } from "react"
import { FaFlag, FaReply } from "react-icons/fa"
import {
  Button,
  Col,
  Form,
  FormGroup,
  FormText,
  Input,
  Label,
} from "reactstrap"
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

  const [bodyError, setBodyError] = useState<string | null>(null)
  const [emailError, setEmailError] = useState<string | null>(null)
  const [websiteError, setWebsiteError] = useState<string | null>(null)

  const { api } = useAPI()
  const [isSubmitting, setIsSubmitting] = useState(false)
  const [body, setBody] = useState("")

  const validate = () => {
    let valid = true
    if (!/\S+@\S+\.\S+/.test(email)) {
      setEmailError("Enter a valid email address.")
      valid = false
    } else {
      setEmailError(null)
    }
    if (!(10 <= body.length && body.length <= 1000)) {
      setBodyError("Body length must be between 10 and 1000 characters.")
      valid = false
    } else {
      setBodyError(null)
    }
    try {
      if (website.length > 0) {
        new URL(website)
      }
      setWebsiteError(null)
    } catch (_) {
      setWebsiteError("Enter a valid URL.")
      valid = false
    }
    return valid
  }

  const applyBackendErrors = (response: any) => {
    response?.author_email?.forEach(setEmailError)
    response?.author_website?.forEach(setWebsiteError)
    response?.content_md?.forEach(setBodyError)
  }

  const submit = async () => {
    try {
      if (!validate()) return
      console.log("Submitting")

      setIsSubmitting(true)
      await api.createComment(
        {
          author_name: name.length > 0 ? name : null,
          author_email: email,
          author_website: website.length > 0 ? website : null,
          content_md: body,
          slug,
        },
        parent
      )
      setBody("")
      onSuccessfullySubmitted()
    } catch (e) {
      applyBackendErrors(e.response.data)
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
          {emailError ? <FormText color="danger">{emailError}</FormText> : null}
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
          {websiteError ? (
            <FormText color="danger">{websiteError}</FormText>
          ) : null}
        </Col>
      </FormGroup>

      <FormGroup>
        <Label for="comment-body">
          Comment ({1000 - body.length} chars left)
        </Label>
        <Input
          disabled={isSubmitting}
          onChange={ev => setBody(ev.target.value)}
          type="textarea"
          name="body"
          value={body}
          id="comment-body"
          placeholder="Type your comment here..."
        />
        <FormText>Markdown formatting is supported.</FormText>
        {bodyError ? <FormText color="danger">{bodyError}</FormText> : null}
      </FormGroup>

      <Button disabled={isSubmitting} color="primary" onClick={submit}>
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
  const user = (
    <>
      <strong>{name}</strong>
      {comment.author_email ? `<${comment.author_email}>` : null}
    </>
  )
  return (
    <div>
      <hr />
      <article className="comment">
        <p className="header">
          {comment.author_website ? (
            <a href={comment.author_website}>{user}</a>
          ) : (
            user
          )}{" "}
          <span className="text-muted">
            on {date.format("MMM DD, YYYY HH:mm:ss")}
          </span>
        </p>
        <div
          className="body"
          dangerouslySetInnerHTML={{ __html: comment.content_html }}
        />
        <div className="actions">
          <p>
            <Button outline color="primary" size="sm" style={{ fontSize: 12 }}>
              <FaReply title="reply" /> Reply
            </Button>{" "}
            <Button outline color="danger" size="sm" style={{ fontSize: 12 }}>
              <FaFlag title="flag" /> Flag as inappropriate
            </Button>
          </p>
        </div>
      </article>
      <div className="children">
        <CommentList comments={comment.children} />
      </div>
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
