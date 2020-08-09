require(`katex/dist/katex.min.css`)
import { Disqus, CommentCount } from "gatsby-plugin-disqus"
import { graphql, Link, PageProps } from "gatsby"
import React, { FC } from "react"
import { Container } from "reactstrap"
import { PostContent, PostMainHeader, PostSEO } from "../components/blog"
import Layout from "../components/layout/layout"
import styleBlog from "../scss/blog.module.scss"
import { BlogPost } from "../types/index"
import { LongformLayout } from "src/components/layout"
import { getPersistentColor } from "src/components/util"
import { getHSLString } from "../components/util"

export const pageQuery = graphql`
  query BlogPostBySlug($slug: String!) {
    allBlogPost(filter: { slug: { eq: $slug } }) {
      edges {
        node {
          description
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

const Sidebar: FC = () => {
  return null
}

const BlogPostTemplate: FC<PageProps<Data, Context>> = ({
  data,
  pageContext,
}) => {
  const post = data.allBlogPost.edges[0].node
  const { previous, next } = pageContext
  const disqusConfig = {
    url: `https://astrid.tech${post.slug}`,
    identifier: post.slug,
    title: post.title,
  }

  return (
    <Layout currentLocation="blog">
      <LongformLayout
        title={post.title}
        description={post.description}
        headingColor={getHSLString(getPersistentColor(post.slug))}
        sidebar={<></>}
        above={
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
        }
      >
        <article dangerouslySetInnerHTML={{ __html: post.source.html }} />
        <section>
          <h2>Comments</h2>
          <Disqus config={disqusConfig} />
        </section>
      </LongformLayout>
    </Layout>
  )
}

export default BlogPostTemplate
