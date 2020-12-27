import { AxiosResponse } from "axios"
import React, { FC, useState } from "react"
import {
  Button,
  Col,
  Form,
  FormGroup,
  FormText,
  Input,
  Label,
  Row,
} from "reactstrap"
import { changeEventSetter, useCookieState } from "src/util/boilerplate"
import { useAPI } from "../APIProvider"
import { useCommentData } from "./CommentDataProvider"

export type CommentingFormProps = {
  replyTo?: number
  onSubmitted?: () => void
}

const COMMENT_EMAIL_COOKIE = "comment-email"
const COMMENT_NAME_COOKIE = "comment-name"
const COMMENT_WEBSITE_COOKIE = "comment-website"
const cookieOptions = {
  maxAge: 30 * 24 * 3600,
  path: "/",
  sameSite: "strict" as "strict",
}

export const CommentingForm: FC<CommentingFormProps> = ({
  replyTo,
  onSubmitted = () => {},
}) => {
  const [
    [name, setName],
    [email, setEmail],
    [website, setWebsite],
  ] = useCookieState(
    [COMMENT_EMAIL_COOKIE, COMMENT_NAME_COOKIE, COMMENT_WEBSITE_COOKIE],
    cookieOptions
  )

  const { slug, refreshComments } = useCommentData()

  const [bodyError, setBodyError] = useState<string | null>(null)
  const [emailError, setEmailError] = useState<string | null>(null)
  const [websiteError, setWebsiteError] = useState<string | null>(null)
  const [generalError, setGeneralError] = useState<string | null>(null)

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

  const applyBackendErrors = (response: AxiosResponse) => {
    const data = response.data
    if (response.status == 403) {
      setGeneralError(`You are unauthorized to post: ${data.detail}`)
    }
    data?.author_email?.forEach(setEmailError)
    data?.author_website?.forEach(setWebsiteError)
    data?.content_md?.forEach(setBodyError)
  }

  const submit = async () => {
    try {
      if (!validate()) return
      setIsSubmitting(true)
      const comment = await api.createComment({
        author_name: name.length > 0 ? name : null,
        author_email: email,
        author_website: website.length > 0 ? website : null,
        content_md: body,
        reply_parent: replyTo,
        slug,
      })
      console.log("Created comment", comment)
      setBody("")
      setIsSubmitting(false)
      onSubmitted()
      await refreshComments()
    } catch (e) {
      applyBackendErrors(e.response)
      setIsSubmitting(false)
    }
  }

  return (
    <div>
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
            {emailError ? (
              <FormText color="danger">{emailError}</FormText>
            ) : null}
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
          {generalError ? (
            <FormText color="danger">{generalError}</FormText>
          ) : null}
        </FormGroup>

        <Row>
          <Button disabled={isSubmitting} color="primary" onClick={submit}>
            Submit!
          </Button>
        </Row>
      </Form>
    </div>
  )
}
