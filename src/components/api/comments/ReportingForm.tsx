import React, { FC, useState } from "react"
import { Button, Form, FormGroup, FormText, Input } from "reactstrap"
import { useAPI } from "../APIProvider"
import { useCommentData } from "./CommentDataProvider"

type ReportFormProps = {
  comment: number
}

export const ReportingForm: FC<ReportFormProps> = ({ comment }) => {
  const [body, setBody] = useState("")
  const { api } = useAPI()
  const { refreshComments } = useCommentData()
  const [isSubmitting, setIsSubmitting] = useState(false)
  const [bodyError, setBodyError] = useState<string | null>(null)

  const applyBackendErrors = (response: any) => {
    response?.content_md?.forEach(setBodyError)
  }

  const submit = async () => {
    try {
      setIsSubmitting(true)
      await api.reportComment(comment, body)
      setBody("")
      refreshComments()
    } catch (e) {
      applyBackendErrors(e.response.data)
    }

    setIsSubmitting(false)
  }

  return (
    <Form>
      <FormGroup>
        <Input
          maxLength={140}
          disabled={isSubmitting}
          onChange={ev => setBody(ev.target.value)}
          type="textarea"
          name="body"
          value={body}
          id="report-body"
          placeholder="Why should this post be removed?"
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
