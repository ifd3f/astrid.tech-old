import React, { FC, ReactNode } from "react"
import { BlogPost, MarkdownBlogPost } from "../types"
import { Link } from "gatsby"
import { TagList } from "./util"
import SEO from "./seo"

type MarkdownPostProps = {
  post: MarkdownBlogPost
}
type AbstractPostProps = {
  post: BlogPost
}

const MarkdownPostContent: FC<MarkdownPostProps> = ({ post }) => {
  return (
    <section dangerouslySetInnerHTML={{ __html: post.markdown!!.html!! }} />
  )
}

export const PostContent: FC<AbstractPostProps> = ({ post }) => {
  switch (post.contentType) {
    case "markdown":
      return <MarkdownPostContent post={post as MarkdownBlogPost} />
    default:
      throw Error(`Unsupported content type ${post.contentType}`)
  }
}

export const PostMainHeader: FC<AbstractPostProps> = ({ post }) => {
  return (
    <header>
      <h1>{post.title!}</h1>
      <p>{new Date(post.date!).toString()}</p>
      <p>{post.description!}</p>
      <TagList tags={post.tags?.map(({ tag }) => tag)} />
    </header>
  )
}

export const MarkdownPostBriefBody: FC<MarkdownPostProps> = ({ post }) => {
  return (
    <section>
      <p
        dangerouslySetInnerHTML={{
          __html: post.markdown!.excerpt!,
        }}
      />{" "}
    </section>
  )
}

const MarkdownBlogPostSEO: FC<MarkdownPostProps> = ({ post }) => {
  return (
    <SEO
      title={post.title!}
      description={post.description || post.markdown?.excerpt || ""}
    />
  )
}

export const PostSEO: FC<AbstractPostProps> = ({ post }) => {
  switch (post.contentType) {
    case "markdown":
      return <MarkdownBlogPostSEO post={post as MarkdownBlogPost} />
      break
    default:
      throw Error(`Unsupported content type ${post.contentType}`)
  }
}

export const PostBrief: FC<AbstractPostProps> = ({ post }) => {
  let body: ReactNode
  switch (post.contentType) {
    case "markdown":
      body = <MarkdownPostBriefBody post={post as MarkdownBlogPost} />
      break
    default:
      throw Error(`Unsupported content type ${post.contentType}`)
  }

  return (
    <article>
      <header>
        <h3>
          <Link to={post.slug!}>{post.title}</Link>
        </h3>
        <small>{new Date(post.date!).toString()}</small>
        <p>{post.description}</p>
        <TagList tags={post.tags!.map(x => x.tag!)} />
      </header>
      {body}
    </article>
  )
}
