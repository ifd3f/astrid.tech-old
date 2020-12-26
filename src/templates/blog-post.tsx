require(`katex/dist/katex.min.css`)
import { graphql, Link, PageProps } from "gatsby"
import React, { createContext, FC, useContext } from "react"
import { FaCalendar } from "react-icons/fa"
import { Container } from "reactstrap"
import { LongformLayout, StatusGroup } from "src/components/layout"
import {
  CommentsRow,
  InfoRow,
  TagsGroup,
} from "src/components/layout/longform-layout"
import { getHSLString, getPersistentColor } from "src/util"
import { CommentSection } from "../components/api/comments/CommentSection"
import Layout from "../components/layout/layout"
import { BlogPost } from "../types/index"

export const pageQuery = graphql`
  query BlogPostBySlug($slug: String!) {
    blogPost(slug: { eq: $slug }) {
      description
      source {
        html
      }
      thumbnail {
        childImageSharp {
          fixed(width: 1200, height: 630, toFormat: PNG, cropFocus: CENTER) {
            src
          }
        }
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
      <InfoRow name="Date" icon={<FaCalendar />}>
        {post.date}
      </InfoRow>
      <CommentsRow disqusConfig={disqusConfig} />
    </StatusGroup>
  )
}

const BlogPostTemplate: FC<PageProps<Data, Context>> = ({
  data,
  pageContext,
  location,
}) => {
  const post = data.blogPost
  const { previous, next } = pageContext
  const url = `${location.origin}${post.slug}`
  const disqusConfig = {
    url,
    identifier: post.slug,
    title: post.title,
  }

  const thumbnail = data.blogPost.thumbnail
    ? `${location.origin}${data.blogPost.thumbnail.childImageSharp.fixed.src}`
    : undefined

  return (
    <ProjectContext.Provider value={{ post, disqusConfig }}>
      <Layout currentLocation="blog">
        <LongformLayout
          thumbnail={thumbnail}
          title={post.title}
          url={url}
          description={post.description}
          descriptionRaw={post.description}
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
            <CommentSection slug={post.slug} />
          </section>
        </Container>
      </Layout>
    </ProjectContext.Provider>
  )
}

export default BlogPostTemplate
