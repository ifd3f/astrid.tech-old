import React from "react"
import { Input, InputGroup } from "reactstrap"
import { generateMarkdownResume } from "./markdown"
import { ResumeGenerator } from "./types"

function TextFieldView({ text }: { text: string }) {
  return (
    <InputGroup>
      <Input value={text} type="textarea" contentEditable="false" />
    </InputGroup>
  )
}

export const TextResumeGenerator: ResumeGenerator = {
  id: "md",
  label: "Markdown",
  generate: (resume, order) => {
    const text = generateMarkdownResume(resume, order)
    return <TextFieldView text={text} />
  },
}

Object.freeze(TextResumeGenerator)
