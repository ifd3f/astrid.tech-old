import { graphql, Link, PageProps } from "gatsby"
import React, { createContext, FC, useContext } from "react"
import { BsArrowLeft } from "react-icons/bs"
import {
  LongformLayout,
  SidebarGroup,
  StatusGroup,
  InfoRow,
  Layout,
  TagsGroup,
} from "src/components/layout"
import {
  getHSLString,
  getPersistentColor,
  formatDateInterval,
} from "src/components/util"
import { StatusBadge } from "../components/project"
import { TagList } from "../components/tag"
import {
  orderByResistorSimilarity,
  BipartiteNode,
} from "../components/tag-similarity/algorithm"
import { BlogPost, Project, Tag } from "../types"
import style from "./project-detail.module.scss"

export const pageQuery = graphql`
  query GetProject($slug: String!) {
    project(slug: { eq: $slug }) {
      slug
      startDate(formatString: "MMMM YYYY")
      endDate(formatString: "MMMM YYYY")
      title
      status
      internal {
        content
        description
      }
      url
      source
      markdown {
        html
      }
      tags {
        ...TagBadge
        tagged {
          ... on Project {
            title
            slug
          }
        }
      }
      childProjectTag {
        childTag {
          tagged {
            __typename
            ... on BlogPost {
              slug
              title
            }
            ... on Work {
              slug
            }
            ... on Course {
              name
              slug
            }
          }
        }
      }
    }
  }
`

type Data = {
  project: Project
}

type Context = {
  id: string
}

const ProjectStatusGroup = () => {
  const { project } = useContext(ProjectContext)
  return (
    <StatusGroup>
      <InfoRow name="Date">
        {formatDateInterval(project.startDate, project.endDate)}
      </InfoRow>
      <InfoRow name="URL">
        <a href={project.url}>{project.url}</a>
      </InfoRow>
      <InfoRow name="Source">
        {project.source.map(url => (
          <p>
            <a href={url}>{url}</a>
          </p>
        ))}
      </InfoRow>
      <InfoRow name="Status">
        <StatusBadge status={project.status} />
      </InfoRow>
    </StatusGroup>
  )
}

const BlogPostsGroup = () => {
  const { project } = useContext(ProjectContext)
  const blogPosts = project.childProjectTag.childTag.tagged.filter(
    item => item.__typename == "BlogPost"
  ) as BlogPost[]

  const list = (
    <ul>
      {blogPosts.map(post => (
        <li key={post.slug}>
          <Link to={post.slug}>{post.title}</Link>
        </li>
      ))}
    </ul>
  )
  return (
    <SidebarGroup>
      <h2>Associated Blog Posts</h2>
      {blogPosts.length == 0 ? <p>N/A</p> : list}
    </SidebarGroup>
  )
}

const RelatedProjectsGroup = () => {
  const { project } = useContext(ProjectContext)

  const neighborNodes: BipartiteNode<Tag, Project>[] = project.tags.map(
    tag => ({
      id: tag.slug,
      neighbors: tag.tagged.map(project => ({
        id: project.slug,
        neighbors: [],
        value: project as Project,
      })),
      value: tag,
    })
  )
  const orderedProjects = orderByResistorSimilarity(neighborNodes).filter(
    other => ![undefined, project.slug].includes(other.value.slug)
  )

  const list = (
    <ul>
      {orderedProjects.slice(0, 5).map(({ value: project }) => (
        <li key={project.slug}>
          <Link to={project.slug}>{project.title}</Link>
        </li>
      ))}
    </ul>
  )

  return (
    <SidebarGroup>
      <h2>Similar Projects</h2>
      {orderedProjects.length == 0 ? <p>N/A</p> : list}
    </SidebarGroup>
  )
}

type ProjectContextData = {
  project: Project
}

const ProjectContext = createContext<ProjectContextData>(
  {} as ProjectContextData
)

const ProjectDetailTemplate: FC<PageProps<Data, Context>> = props => {
  const { data } = props
  const project = data.project

  return (
    <ProjectContext.Provider value={{ project }}>
      <Layout {...props} currentLocation="projects">
        <LongformLayout
          title={project.title}
          description={project.internal.description}
          descriptionRaw={project.internal.description}
          headingColor={getHSLString(getPersistentColor(project.slug))}
          above={
            <Link to="/projects" className={style.backToProjects}>
              <BsArrowLeft /> Back to Projects
            </Link>
          }
          sidebar={
            <>
              <ProjectStatusGroup />
              <TagsGroup tags={project.tags} />
              <BlogPostsGroup />
              <RelatedProjectsGroup />
            </>
          }
        >
          <article
            className="longform"
            dangerouslySetInnerHTML={{ __html: project.markdown.html!! }}
          />
        </LongformLayout>
      </Layout>
    </ProjectContext.Provider>
  )
}

export default ProjectDetailTemplate
