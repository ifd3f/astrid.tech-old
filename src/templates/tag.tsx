import { graphql, PageProps } from "gatsby"
import React, { FC } from "react"
import Layout from "../components/layout/layout"
import { Tag, Project, WorkExperience, BlogPost } from "../types/index"
import { TagBadge } from "../components/tag"
import SEO from "../components/seo"
import { Container } from "reactstrap"
import { ProjectCard } from "../components/project"
import { PostBrief } from "../components/blog"

export const pageQuery = graphql`
  query GetTag($slug: String!) {
    allTag(filter: { slug: { eq: $slug } }) {
      edges {
        node {
          slug
          name
          color
          textColor
        }
      }
    }
    allProject(
      filter: { tags: { elemMatch: { slug: { eq: $slug } } } }
      sort: { fields: [endDate], order: DESC }
    ) {
      edges {
        node {
          slug
          thumbnailPublicPath
          startDate(formatString: "YYYY-MM")
          endDate(formatString: "YYYY-MM")
          title
          description
          status
          markdown {
            html
          }
          tags {
            tag {
              name
              color
              textColor
              slug
            }
          }
          url
          source
        }
      }
    }
    allWorkExperience(
      filter: { tags: { elemMatch: { slug: { eq: $slug } } } }
    ) {
      edges {
        node {
          slug
        }
      }
    }
    allBlogPost(filter: { tags: { elemMatch: { slug: { eq: $slug } } } }) {
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

type Data = {
  allTag: {
    edges: {
      node: Tag
    }[]
  }
  allProject: {
    edges: {
      node: Project
    }[]
  }
  allWorkExperience: {
    edges: {
      node: WorkExperience
    }[]
  }
  allBlogPost: {
    edges: {
      node: BlogPost
    }[]
  }
}

type Context = {
  id: string
}

const TagDetailTemplate: FC<PageProps<Data, Context>> = ({ data }) => {
  const tag = data.allTag.edges[0].node

  const postsEmpty = data.allBlogPost.edges.length == 0
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
  )

  return (
    <Layout>
      <SEO title={tag.name!} />
      <Container tag="article">
        <header>
          <h1>
            <TagBadge tag={tag} />
          </h1>
        </header>
        {postsEmpty ? null : postsSection()}
        {projectsEmpty ? null : projectsSection()}
      </Container>
    </Layout>
  )
}

export default TagDetailTemplate
