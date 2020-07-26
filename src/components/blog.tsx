import { Link } from "gatsby"
import moment from "moment"
import React, { FC } from "react"
import style from "../scss/blog.module.scss"
import { BlogPost } from "../types"
import SEO from "./seo"
import { TagList } from "./tag"

type PostProps = {
  post: BlogPost
}

export const PostContent: FC<PostProps> = ({ post }) => {
  return <section dangerouslySetInnerHTML={{ __html: post.source.html }} />
}

export const PostMainHeader: FC<PostProps> = ({ post }) => {
  const dateString = moment(post.date).format("h:mma, dddd DD MMMM YYYY")
  return (
    <header>
      <h1>{post.title!}</h1>
      <p className={style.subtitle}>{post.internal.description!}</p>
      <p className={style.date}>{dateString}</p>
      <TagList tags={post.tags} link />
    </header>
  )
}

export const PostSEO: FC<PostProps> = ({ post }) => {
  return <SEO title={post.title!} description={post.internal.description} />
}

export const PostBrief: FC<PostProps> = ({ post }) => {
  const dateString = moment(post.date).format("DD MMMM YYYY")

  return (
    <Link to={post.slug!}>
      <article className={style.brief}>
        <h3>{post.title}</h3>
        <p className={style.date}>{dateString}</p>
        <TagList tags={post.tags} link />
        <p>{post.internal.description}</p>
      </article>
    </Link>
  )
}
