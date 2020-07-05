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
          parent {
            ... on MarkdownRemark {
              html
              excerpt
            }
          }
          title
          date
          slug
          description
          contentType
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

const BlogIndex: FC<PageProps<Data>> = props => {
  const { data } = props
  return (
    <Layout {...props}>
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
