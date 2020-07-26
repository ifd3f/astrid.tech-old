import { Project, BlogPost } from "../types"
import { PageProps, graphql, Link } from "gatsby"
import { FC, PropsWithChildren, createContext, useContext } from "react"
import Layout from "../components/layout/layout"
import React from "react"
import SEO from "../components/seo"
import { Container, Col, Row } from "reactstrap"
import { TagList } from "../components/tag"
import { StatusBadge } from "../components/project"
import style from "./project-detail.module.scss"
import { BsArrowLeft } from "react-icons/bs"

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

const SidebarGroup: FC<PropsWithChildren<{}>> = ({ children }) => (
  <div className={style.sidebarGroup}>{children}</div>
)

function formatDateInterval(startDate: string, endDate: string) {
  if (startDate == endDate) {
    return startDate
  }
  return endDate ? `${startDate} to ${endDate}` : `${startDate} to now`
}

const StatusGroup = () => {
  const { project } = useContext(ProjectContext)
  return (
    <SidebarGroup>
      <table style={{ width: "100%" }}>
        <tr>
          <th>Date</th>
          <td className={style.statusData}>
            {formatDateInterval(project.startDate, project.endDate)}
          </td>
        </tr>
        {project.status ? (
          <tr>
            <th>Status</th>
            <td className={style.statusData}>
              <StatusBadge status={project.status} />
            </td>
          </tr>
        ) : null}
        {project.url ? (
          <tr>
            <th>URL</th>
            <td className={style.statusData}>
              <a href={project.url}>{project.url}</a>
            </td>
          </tr>
        ) : null}
        {project.source.length > 0 ? (
          <tr>
            <th>Source</th>
            <td className={style.statusData}>
              {project.source.map(url => (
                <p>
                  <a href={url}>{url}</a>
                </p>
              ))}
            </td>
          </tr>
        ) : null}
      </table>
    </SidebarGroup>
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
      <h2>Related Blog Posts</h2>
      {blogPosts.length == 0 ? <p>N/A</p> : list}
    </SidebarGroup>
  )
}

const TagsGroup = () => {
  const { project } = useContext(ProjectContext)
  return (
    <SidebarGroup>
      <h2>Tags</h2>
      <TagList tags={project.tags} link />
    </SidebarGroup>
  )
}

const RelatedProjectsGroup = () => {
  const { project } = useContext(ProjectContext)

  // Consider the hypergraph where projects are vertices and tags are edges.

  const score = new Map<string, number>()
  const slugToProject = new Map<string, Project>()

  for (let tag of project.tags) {
    const cardinality = tag.tagged.length
    for (let item of tag.tagged) {
      if (item.__typename != "Project") continue
      const other = item as Project
      if (project.slug == other.slug) continue

      const currentScore = score.get(other.slug) ?? 0
      score.set(other.slug, 1 / cardinality + currentScore)
      slugToProject.set(other.slug, other)
    }
  }

  const projects = []
  for (let [slug, project] of slugToProject) {
    console.log(slug, score.get(slug))
    //score.set(slug, connectivity.get(slug)! + tagCardinality.get(slug)!)
    projects.push(project)
  }

  projects.sort((a, b) => score.get(b.slug)! - score.get(a.slug)!)

  const list = (
    <ul>
      {projects.slice(0, 5).map(project => (
        <li key={project.slug}>
          <Link to={project.slug}>{project.title}</Link>
        </li>
      ))}
    </ul>
  )

  return (
    <SidebarGroup>
      <h2>Similar Projects</h2>
      {projects.length == 0 ? <p>N/A</p> : list}
    </SidebarGroup>
  )
}

const Sidebar: FC = () => {
  return (
    <>
      <StatusGroup />
      <TagsGroup />
      <RelatedProjectsGroup />
      <BlogPostsGroup />
    </>
  )
}

const Content: FC = () => {
  const { project } = useContext(ProjectContext)
  return (
    <>
      <header>
        <h1>{project.title!}</h1>
        <p className={style.subtitle}>{project.internal.description}</p>
      </header>
      <section dangerouslySetInnerHTML={{ __html: project.markdown.html!! }} />
    </>
  )
}

type ProjectContextData = {
  project: Project
}

const ProjectContext = createContext<ProjectContextData>(
  {} as ProjectContextData
)

const ProjectDetailTemplate: FC<PageProps<Data, Context>> = ({ data }) => {
  const project = data.project

  return (
    <ProjectContext.Provider value={{ project }}>
      <Layout currentLocation="projects">
        <SEO title={project.title!} />
        <Container tag="article" className={style.projectDetailContainer}>
          <nav>
            <Link to="/projects" className={style.backToProjects}>
              <BsArrowLeft /> Back to Projects
            </Link>
          </nav>
          <Row>
            <Col lg={8} className={style.content}>
              <Content />
            </Col>
            <Col lg={4}>
              <Sidebar />
            </Col>
          </Row>
        </Container>
      </Layout>
    </ProjectContext.Provider>
  )
}

export default ProjectDetailTemplate
