import { graphql, PageProps } from "gatsby"
import React, { FC } from "react"
import { Container } from "reactstrap"
import Layout from "../components/layout/layout"
import SEO from "../components/seo"
import { TagBadge } from "../components/tag"
import { Tag, Project, BlogPost } from "src/types"

export const pageQuery = graphql`
  query GetTagInfo($slug: String!) {
    tag(slug: { eq: $slug }) {
      ...TagBadge
      tagged {
        __typename
        ... on Project {
          title
          slug
          startDate
          endDate
        }
        ... on BlogPost {
          title
          slug
          date
        }
      }
    }
  }
`

type Data = {
  tag: Tag & {
    projects: Project[]
    blogPosts: BlogPost[]
  }
}

type SiteObject<T, TypeString> = T & {
  sortDate: Date
  __typename: TypeString
}

type SiteObjectUnion =
  | SiteObject<Project, "Project">
  | SiteObject<BlogPost, "BlogPost">

type Context = {
  id: string
}

type SiteObjectDisplayProps = {
  object: SiteObjectUnion
}

function convertDateStringInterval(start: string, end?: string | null) {
  if (end) {
    return new Date(end)
  }
  const midpoint = (new Date(start).getTime() + new Date().getTime()) / 2
  return new Date(midpoint)
}

const BlogPostDisplay: FC<{ post: BlogPost }> = ({ post }) => {
  return (
    <p>
      {post.title} {post.date}
    </p>
  )
}

const ProjectDisplay: FC<{ project: Project }> = ({ project }) => {
  return (
    <p>
      {project.title} {project.startDate}
    </p>
  )
}
const SiteObjectDisplay: FC<SiteObjectDisplayProps> = ({ object }) => {
  switch (object.__typename) {
    case "BlogPost":
      return <BlogPostDisplay post={object} />
    case "Project":
      return <ProjectDisplay project={object} />
  }
  return <p>invalid</p>
}

const TagDetailTemplate: FC<PageProps<Data, Context>> = ({ data: { tag } }) => {
  const projects = (tag.tagged.filter(
    t => t.__typename == "Project"
  ) as Project[]).map(
    project =>
      ({
        ...project,
        sortDate: convertDateStringInterval(project.startDate, project.endDate),
      } as SiteObjectUnion)
  )

  const blogPosts = (tag.tagged.filter(
    t => t.__typename == "BlogPost"
  ) as BlogPost[]).map(
    post =>
      ({
        ...post,
        sortDate: new Date(post.date),
      } as SiteObjectUnion)
  )

  const objects = projects
    .concat(blogPosts)
    .sort((a, b) => b.sortDate.getTime() - a.sortDate.getTime())

  return (
    <Layout>
      <SEO title={tag.name!} description={`Items related to ${tag.name}`} />
      <Container tag="article">
        <header>
          <h1>
            Items related to <TagBadge tag={tag} />
          </h1>
        </header>

        <section>
          {objects.map(object => (
            <SiteObjectDisplay object={object} />
          ))}
        </section>
      </Container>
    </Layout>
  )
}

export default TagDetailTemplate
