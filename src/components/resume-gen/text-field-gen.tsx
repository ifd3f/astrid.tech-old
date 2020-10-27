import React from "react"
import { Input, InputGroup } from "reactstrap"
import { Resume, ResumeGenerator } from "./types"

function TextFieldView({ text }: { text: string }) {
  return (
    <InputGroup>
      <Input
        value={text}
        type="textarea"
        contentEditable="false"
        style={{ height: 500 }}
      />
    </InputGroup>
  )
}

export class TextResumeGenerator implements ResumeGenerator {
  constructor(
    public readonly ext: string,
    public readonly label: string,
    private readonly generator: (resume: Resume, order: string) => string
  ) {}

  generate(resume: Resume, order: string) {
    const text = this.generator(resume, order)
    return <TextFieldView text={text} />
  }
}
