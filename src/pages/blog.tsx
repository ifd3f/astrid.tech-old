import React from "react"
import { PageProps, Link, graphql } from "gatsby"

import Bio from "../components/bio"
import Layout from "../components/layout"
import SEO from "../components/seo"
import { rhythm } from "../utils/typography"
import { BlogPost } from "../types/index"
import { PostBrief } from "../components/blog"

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
    allBlogPost {
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
  const posts = data.allBlogPost.edges

  return (
    <Layout>
      <SEO title="Blog" />
      <Bio />
      {posts.map(({ node }) => (
        <PostBrief data={node} />
      ))}
    </Layout>
  )
}

export default BlogIndex
