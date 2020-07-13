import { graphql, PageProps } from "gatsby"
import React, { FC } from "react"
import { Container } from "reactstrap"
import Layout from "../components/layout/layout"
import SEO from "../components/seo"
import { TagBadge } from "../components/tag"
import { Tag } from "../types/index"

export const pageQuery = graphql`
  query GetTagInfo($slug: String!) {
    allTag(filter: { slug: { eq: $slug } }) {
      edges {
        node {
          ...TagBadge
          tagged {
            __typename
            ... on Work {
              id
            }
            ... on Project {
              id
            }
            ... on Course {
              id
              name
            }
            ... on BlogPost {
              id
              date
            }
          }
        }
      }
    }
  }
`

type Data = {
  allTag: {
    edges: [
      {
        node: Tag
      }
    ]
  }
}

type Context = {
  id: string
}

const TagDetailTemplate: FC<PageProps<Data, Context>> = ({ data }) => {
  const tag = data.allTag.edges[0].node
  console.log(data)

  /*
  const postsEmpty = tag.allBlogPost.edges.length == 0
  const postsSection = () => (
    <section>
      <h2>Blog Posts</h2>
      {data.allBlogPost.edges.map(({ node: post }) => (
        <PostBrief post={post} />
      ))}
    </section>
  )

  const projectsEmpty = data.allProject.edges.length == 0
  const projectsSection = () => (
    <section>
      <h2>Projects</h2>
      {data.allProject.edges.map(({ node: project }) => (
        <ProjectCard project={project} />
      ))}
    </section>
  )*/

  return (
    <Layout>
      <SEO title={tag.name!} />
      <Container tag="article">
        <header>
          <h1>
            <TagBadge tag={tag} />
          </h1>
        </header>
      </Container>
    </Layout>
  )
}

export default TagDetailTemplate
