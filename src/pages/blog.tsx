import { graphql, PageProps } from "gatsby"
import React, { FC } from "react"
import { PostBrief } from "../components/blog"
import Layout from "../components/layout/layout"
import SEO from "../components/seo"
import { BlogPost } from "../types/index"
import { Container } from "reactstrap"
import styles from "../scss/blog.module.scss"

type Data = {
  allBlogPost: {
    edges: [
      {
        node: BlogPost
      }
    ]
  }
}

export const pageQuery = graphql`
  query {
    allBlogPost(sort: { fields: date, order: DESC }) {
      edges {
        node {
          internal {
            description
            content
          }
          title
          date
          slug
          tags {
            ...TagBadge
          }
        }
      }
    }
  }
`

const BlogIndex: FC<PageProps<Data>> = props => {
  const { data } = props
  return (
    <Layout {...props} currentLocation="blog">
      <SEO
        title="Blog"
        description="Astrid Yu's Designated Mind Dump Location"
      />
      <Container className={styles.blogContentContainer}>
        <h1>Blog</h1>
        {data.allBlogPost.edges.map(({ node: post }) => (
          <PostBrief post={post} />
        ))}
      </Container>
    </Layout>
  )
}

export default BlogIndex
