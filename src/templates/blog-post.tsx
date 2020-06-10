import React, { FC } from "react"
import { Link, graphql, PageProps } from "gatsby"

import Bio from "../components/bio"
import Layout from "../components/layout"
import SEO from "../components/seo"
import { rhythm, scale } from "../utils/typography"
import { PostMainHeader, PostSEO, PostContent } from "../components/blog"
import { BlogPost } from "../types/index"

export const pageQuery = graphql`
  query BlogPostBySlug($id: String!) {
    allBlogPost(filter: { id: { eq: $id } }) {
      edges {
        node {
          title
          date
          slug
          description
          contentType
          markdown {
            html
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

type Data = {
  allBlogPost: {
    edges: [
      {
        node: BlogPost
      }
    ]
  }
}

type Context = {
  previous: BlogPost
  next: BlogPost
}

const BlogPostTemplate: FC<PageProps<Data, Context>> = ({
  data,
  pageContext,
}) => {
  const post = data.allBlogPost.edges[0].node
  const { previous, next } = pageContext

  return (
    <Layout>
      <PostSEO post={post} />
      <article>
        <PostMainHeader post={post} />
        <PostContent post={post} />
        <hr
          style={{
            marginBottom: rhythm(1),
          }}
        />
        <footer>
          <Bio />
        </footer>
      </article>

      <nav>
        <ul
          style={{
            display: `flex`,
            flexWrap: `wrap`,
            justifyContent: `space-between`,
            listStyle: `none`,
            padding: 0,
          }}
        >
          <li>
            {previous && (
              <Link to={previous.slug!} rel="prev">
                ← {previous.title}
              </Link>
            )}
          </li>
          <li>
            {next && (
              <Link to={next.slug!} rel="next">
                {next.title} →
              </Link>
            )}
          </li>
        </ul>
      </nav>
    </Layout>
  )
}

export default BlogPostTemplate
