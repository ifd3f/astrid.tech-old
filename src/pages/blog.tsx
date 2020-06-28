import { graphql, PageProps } from "gatsby"
import React from "react"
import { PostBrief } from "../components/blog"
import Layout from "../components/layout"
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
          title
          date
          slug
          description
          contentType
          markdown {
            excerpt
          }
          tags {
            tag {
              slug
              name
              color
              textColor
            }
          }
        }
      }
    }
  }
`

const BlogIndex = ({ data }: PageProps<Data>) => {
  return (
    <Layout>
      <SEO title="Blog" />
      <Container className={styles.blogContentContainer}>
        {data.allBlogPost.edges.map(({ node: post }) => (
          <PostBrief post={post} />
        ))}
      </Container>
    </Layout>
  )
}

export default BlogIndex
