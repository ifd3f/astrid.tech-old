import React, { FC, useEffect, useState } from "react"
import { Button, Col, Form, FormGroup, Input, Label } from "reactstrap"
import { CommentData } from "src/astrid-tech-api"
import { useAPI } from "./APIProvider"

export type CommentingFormProps = {
  slug: string
  parent?: number
  onSuccessfullySubmitted?: () => {}
}

const CommentingForm: FC<CommentingFormProps> = ({
  slug,
  parent,
  onSuccessfullySubmitted = () => {},
}) => {
  const { api } = useAPI()
  const [isSubmitting, setIsSubmitting] = useState(false)
  const [name, setName] = useState("")
  const [email, setEmail] = useState("")
  const [body, setBody] = useState("")

  const submit = async () => {
    try {
      setIsSubmitting(true)
      await api.createComment(
        {
          author_name: name,
          author_email: email,
          content_md: body,
          slug,
        },
        parent
      )
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
            onChange={ev => setName(ev.target.value)}
            type="text"
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
            onChange={ev => setEmail(ev.target.value)}
            type="email"
            name="email"
            id="comment-email"
            placeholder="user@example.com"
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
  return (
    <div>
      <div>
        <p>{comment.author_name}</p>
      </div>
      <div dangerouslySetInnerHTML={{ __html: comment.content_html }}></div>
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
    <>
      <CommentingForm slug={slug} onSuccessfullySubmitted={fetchComments} />
      {comments ? (
        comments.length > 0 ? (
          <CommentList comments={comments} slug={slug} />
        ) : (
          <p>No comments. Start the conversation!</p>
        )
      ) : (
        <p>Loading comments...</p>
      )}
    </>
  )
}
