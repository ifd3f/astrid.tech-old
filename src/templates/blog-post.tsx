import { graphql, Link, PageProps } from "gatsby"
import React, { FC } from "react"
import { PostContent, PostMainHeader, PostSEO } from "../components/blog"
import Layout from "../components/layout"
import { BlogPost } from "../types/index"
import { Container } from "reactstrap"
import styleBlog from "../scss/blog.module.scss"
import Helmet from "react-helmet"

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
          jupyter {
            html
          }
          tags {
            tag {
              ...TagBadge
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
      <Helmet>
        <link
          rel="stylesheet"
          href="https://cdn.jsdelivr.net/npm/katex@0.11.1/dist/katex.min.css"
          integrity="sha384-zB1R0rpPzHqg7Kpt0Aljp8JPLqbXI3bhnPWROx27a9N0Ll6ZP/+DiW/UqRcLbRjq"
          crossorigin="anonymous"
        />

        <script
          defer
          src="https://cdn.jsdelivr.net/npm/katex@0.11.1/dist/katex.min.js"
          integrity="sha384-y23I5Q6l+B6vatafAwxRu/0oK/79VlbSz7Q9aiSZUvyWYIYsd+qj+o24G5ZU2zJz"
          crossorigin="anonymous"
        ></script>
      </Helmet>
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
