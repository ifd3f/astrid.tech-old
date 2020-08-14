require(`katex/dist/katex.min.css`)
import { graphql, Link, PageProps } from "gatsby"
import { Disqus, CommentCount } from "gatsby-plugin-disqus"
import React, { createContext, FC, useContext, ReactNode } from "react"
import { LongformLayout, StatusGroup } from "src/components/layout"
import { getPersistentColor } from "src/components/util"
import Layout from "../components/layout/layout"
import { getHSLString } from "../components/util"
import { BlogPost } from "../types/index"
import { InfoRow, TagsGroup } from "src/components/layout/longform-layout"
import { Container } from "reactstrap"

export const pageQuery = graphql`
  query BlogPostBySlug($slug: String!) {
    blogPost(slug: { eq: $slug }) {
      description {
        childMarkdownRemark {
          html
        }
      }
      source {
        html
      }
      title
      date(formatString: "YYYY MMMM DD")
      slug
      tags {
        ...TagBadge
      }
    }
  }
`

type Data = {
  blogPost: BlogPost
}

type Context = {
  previous: BlogPost
  next: BlogPost
}

type PostContextData = {
  post: BlogPost
  disqusConfig: any
}

const ProjectContext = createContext<PostContextData>({} as PostContextData)

const PostStatusGroup: FC = () => {
  const { post, disqusConfig } = useContext(ProjectContext)
  return (
    <StatusGroup>
      <InfoRow name="Date">{post.date}</InfoRow>
      <InfoRow name="Comments">
        <a href="#comments">
          <CommentCount config={disqusConfig} />
        </a>
      </InfoRow>
    </StatusGroup>
  )
}

const BlogPostTemplate: FC<PageProps<Data, Context>> = ({
  data,
  pageContext,
}) => {
  const post = data.blogPost
  const { previous, next } = pageContext
  const disqusConfig = {
    url: `https://astrid.tech${post.slug}`,
    identifier: post.slug,
    title: post.title,
  }

  return (
    <ProjectContext.Provider value={{ post, disqusConfig }}>
      <Layout currentLocation="blog">
        <LongformLayout
          title={post.title}
          description={
            <span
              dangerouslySetInnerHTML={{
                __html: post.description.childMarkdownRemark.html,
              }}
            />
          }
          descriptionRaw={post.description.text}
          headingColor={getHSLString(getPersistentColor(post.slug))}
          sidebar={
            <>
              <PostStatusGroup />
              <TagsGroup tags={post.tags} />
            </>
          }
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
          <article
            className="longform"
            dangerouslySetInnerHTML={{ __html: post.source.html }}
          />
        </LongformLayout>
        <Container>
          <section id="comments">
            <h2>Comments</h2>
            <Disqus config={disqusConfig} />
          </section>
        </Container>
      </Layout>
    </ProjectContext.Provider>
  )
}

export default BlogPostTemplate
