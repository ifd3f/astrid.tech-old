require(`katex/dist/katex.min.css`)
import { graphql, Link, PageProps } from "gatsby"
import React, { FC } from "react"
import { Container } from "reactstrap"
import { PostContent, PostMainHeader, PostSEO } from "../components/blog"
import Layout from "../components/layout/layout"
import styleBlog from "../scss/blog.module.scss"
import { BlogPost } from "../types/index"

export const pageQuery = graphql`
  query BlogPostBySlug($slug: String!) {
    allBlogPost(filter: { slug: { eq: $slug } }) {
      edges {
        node {
          internal {
            description
          }
          source {
            html
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

      <Container className={styleBlog.blogContentContainer}>
        <article>
          <PostMainHeader post={post} />
          <PostContent post={post} />
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
      </Container>
    </Layout>
  )
}

export default BlogPostTemplate
