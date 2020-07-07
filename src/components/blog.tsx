import React, { FC, ReactNode } from "react"
import { BlogPost, MarkdownBlogPost, JupyterBlogPost } from "../types"
import moment from "moment"
import { Link } from "gatsby"
import { TagList } from "./tag"
import SEO from "./seo"
import style from "../scss/blog.module.scss"

type MarkdownPostProps = {
  post: MarkdownBlogPost
}
type JupyterPostProps = {
  post: JupyterBlogPost
}
type AbstractPostProps = {
  post: BlogPost
}

const MarkdownPostContent: FC<MarkdownPostProps> = ({ post }) => {
  return <section dangerouslySetInnerHTML={{ __html: post.parent!!.html!! }} />
}

const JupyterPostContent: FC<JupyterPostProps> = ({ post }) => {
  return (
    <section
      dangerouslySetInnerHTML={{ __html: post.parent!!.internal!!.content!! }}
    />
  )
}

export const PostContent: FC<AbstractPostProps> = ({ post }) => {
  switch (post.contentType) {
    case "markdown":
      return <MarkdownPostContent post={post as MarkdownBlogPost} />
    case "jupyter":
      return <JupyterPostContent post={post as JupyterBlogPost} />
    default:
      throw Error(`Unsupported content type ${post.contentType}`)
  }
}

export const PostMainHeader: FC<AbstractPostProps> = ({ post }) => {
  const dateString = moment(post.date).format("h:mma, dddd DD MMMM YYYY")
  return (
    <header>
      <h1>{post.title!}</h1>
      <p className={style.subtitle}>{post.description!}</p>
      <p className={style.date}>{dateString}</p>
      <TagList tags={post.tags!.map(({ tag }) => tag!)} />
    </header>
  )
}

export const MarkdownPostBriefBody: FC<MarkdownPostProps> = ({ post }) => {
  return (
    <section>
      <p
        dangerouslySetInnerHTML={{
          __html: post.parent!.excerpt!,
        }}
      />{" "}
    </section>
  )
}

const MarkdownBlogPostSEO: FC<MarkdownPostProps> = ({ post }) => {
  return (
    <SEO
      title={post.title!}
      description={post.description || post.parent?.excerpt || ""}
    />
  )
}

export const PostSEO: FC<AbstractPostProps> = ({ post }) => {
  switch (post.contentType) {
    case "markdown":
      return <MarkdownBlogPostSEO post={post as MarkdownBlogPost} />
    case "jupyter":
      return null
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
    case "jupyter":
      break
    default:
      throw Error(`Unsupported content type ${post.contentType}`)
  }

  const dateString = moment(post.date).format("DD MMMM YYYY")

  return (
    <Link to={post.slug!}>
      <article className={style.brief}>
        <h3>{post.title}</h3>
        <p className={style.date}>{dateString}</p>
        <TagList tags={post.tags!.map(x => x.tag!)} />
        <p>{post.description}</p>
      </article>
    </Link>
  )
}
