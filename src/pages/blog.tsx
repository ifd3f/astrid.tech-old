import { graphql, PageProps } from "gatsby"
import React, { FC } from "react"
import { FaRssSquare } from "react-icons/fa"
import { Container } from "reactstrap"
import { PageHeading } from "src/components/layout"
import { PostBrief } from "../components/blog"
import Layout from "../components/layout/layout"
import SEO from "../components/seo"
import styles from "../scss/blog.module.scss"
import { BlogPost } from "../types/index"

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
          ...PostBrief
        }
      }
    }
  }
`

const BlogIndex: FC<PageProps<Data>> = props => {
  const { data } = props
  const title = "Blog"
  const description = "Astrid Yu's designated mind dump location"
  return (
    <Layout {...props} currentLocation="blog">
      <SEO title={title} description={description} />
      <PageHeading title={title} description={description} bgColor="#eecc8d" />
      <Container className={styles.blogContentContainer}>
        <section>
          <p className="text-right">
            <a href="https://astrid.tech/rss.xml">
              <FaRssSquare title="Subscribe to the Blog!" />
            </a>
          </p>
        </section>
        <section>
          {data.allBlogPost.edges.map(({ node: post }) => (
            <PostBrief key={post.slug} post={post} />
          ))}
          <p className="text-center text-muted">(End of posts)</p>
        </section>
      </Container>
    </Layout>
  )
}

export default BlogIndex
