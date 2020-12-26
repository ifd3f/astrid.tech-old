import React from "react"
import { Button, Input, InputGroup } from "reactstrap"
import { Resume, ResumeGenerator } from "./types"

function TextFieldView({ text }: { text: string }) {
  return (
    <InputGroup>
      <Input
        onClick={ev => {
          ;(ev.target as any).select()
        }}
        value={text}
        type="textarea"
        style={{ height: 500 }}
        readonly
      />
    </InputGroup>
  )
}

function downloadString(text: string, fileType: string, fileName: string) {
  var blob = new Blob([text], { type: fileType })

  var a = document.createElement("a")
  a.download = fileName
  a.href = URL.createObjectURL(blob)
  a.dataset.downloadurl = [fileType, a.download, a.href].join(":")
  a.style.display = "none"
  document.body.appendChild(a)
  a.click()
  document.body.removeChild(a)
  setTimeout(function () {
    URL.revokeObjectURL(a.href)
  }, 1500)
}

export class TextResumeGenerator implements ResumeGenerator {
  constructor(
    public readonly ext: string,
    public readonly label: string,
    private readonly generator: (resume: Resume, order: string) => string
  ) {}

  generate(resume: Resume, order: string) {
    const text = this.generator(resume, order)
    return (
      <>
        <Button
          onClick={() => downloadString(text, this.ext, "resume." + this.ext)}
        >
          Download
        </Button>
        <TextFieldView text={text} />{" "}
      </>
    )
  }
}
