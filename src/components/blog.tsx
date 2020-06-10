import React, { FC, ReactNode } from "react"
import { BlogPost, MarkdownBlogPost } from "../types"
import { Link } from "gatsby"

type MarkdownPostProps = {
  data: MarkdownBlogPost
}
type AbstractPostProps = {
  data: BlogPost
}

const MarkdownPostContent: FC<MarkdownPostProps> = ({ data }) => {
  return null
}

export const PostContent: FC<AbstractPostProps> = ({ data }) => {
  switch (data.contentType) {
    case "markdown":
      return <MarkdownPostContent data={data as MarkdownBlogPost} />
    default:
      throw Error(`Unsupported content type ${data.contentType}`)
  }
}

export const MarkdownPostBriefBody: FC<MarkdownPostProps> = ({ data }) => {
  return (
    <section>
      <p
        dangerouslySetInnerHTML={{
          __html: data.markdown!.excerpt,
        }}
      />{" "}
    </section>
  )
}

export const PostBrief: FC<AbstractPostProps> = ({ data }) => {
  let body: ReactNode
  switch (data.contentType) {
    case "markdown":
      body = <MarkdownPostBriefBody data={data as MarkdownBlogPost} />
      break
    default:
      throw Error(`Unsupported content type ${data.contentType}`)
  }

  return (
    <article>
      <header>
        <h3>
          <Link to={data.slug!}>{data.title}</Link>
        </h3>
        <small>{data.date}</small>
        <p>{data.description}</p>
      </header>
      {body}
    </article>
  )
}
