import { graphql, Link } from "gatsby"
import moment from "moment"
import React, { FC } from "react"
import { Col, Row } from "reactstrap"
import style from "../scss/blog.module.scss"
import { BlogPost } from "../types"
import { TagList } from "./tag"

type PostProps = {
  post: BlogPost
}

export const PostBriefFragment = graphql`
  fragment PostBrief on BlogPost {
    description
    title
    date
    slug
    tags {
      ...TagBadge
    }
    source {
      excerpt
    }
  }
`
export const PostBrief: FC<PostProps> = ({ post }) => {
  const dateString = moment(post.date).format("DD MMMM YYYY")

  return (
    <Link to={post.slug!}>
      <article className={style.brief}>
        <Row className="col">
          <Col sm={8}>
            <h3>{post.title}</h3>
            <div className="text-muted">{post.description}</div>
          </Col>
          <Col sm={4}>
            <p className={`text-muted ${style.date}`}>{dateString}</p>
            <TagList tags={post.tags} link limit={5} />
          </Col>
        </Row>
        <Col>
          <p>{post.source.excerpt}</p>
        </Col>
      </article>
    </Link>
  )
}
