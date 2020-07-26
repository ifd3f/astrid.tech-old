import { Project } from "../types"
import { PageProps, graphql } from "gatsby"
import { FC, PropsWithChildren } from "react"
import Layout from "../components/layout/layout"
import React from "react"
import SEO from "../components/seo"
import { Container, Col, Row } from "reactstrap"
import { TagList } from "../components/tag"
import { StatusBadge } from "../components/project"
import style from "./project-detail.module.scss"

export const pageQuery = graphql`
  query GetProject($slug: String!) {
    allProject(filter: { slug: { eq: $slug } }) {
      edges {
        node {
          slug
          startDate(formatString: "MMMM YYYY")
          endDate(formatString: "MMMM YYYY")
          title
          status
          internal {
            content
            description
          }
          tags {
            ...TagBadge
          }
          url
          source
          markdown {
            html
          }
        }
      }
    }
  }
`

type Data = {
  allProject: {
    edges: [
      {
        node: Project
      }
    ]
  }
}

type Context = {
  id: string
}

// TODO betterize
const SidebarGroup: FC<PropsWithChildren<{}>> = ({ children }) => (
  <div>{children}</div>
)

type SidebarProps = {
  project: Project
}
const Sidebar: FC<SidebarProps> = ({ project }) => {
  return (
    <div>
      <SidebarGroup>
        <h2>Dates</h2>
        <p className={style.date}>
          {project.endDate
            ? `${project.startDate} to ${project.endDate}`
            : project.startDate}
        </p>
      </SidebarGroup>
      <SidebarGroup>
        <h2>Status</h2>
        <StatusBadge status={project.status} />
      </SidebarGroup>
      <SidebarGroup>
        <h2>Tags</h2>
        <TagList tags={project.tags} link />
      </SidebarGroup>
      <SidebarGroup>
        <h2>Related Blog Posts</h2>
      </SidebarGroup>
      <SidebarGroup>
        <h2>Similar Projects</h2>
      </SidebarGroup>
    </div>
  )
}

const ProjectDetailTemplate: FC<PageProps<Data, Context>> = ({ data }) => {
  const project = data.allProject.edges[0].node

  return (
    <Layout currentLocation="projects">
      <SEO title={project.title!} />
      <Container tag="article" className={style.projectDetailContainer}>
        <Row>
          <Col lg={8}>
            <header>
              <h1>{project.title!}</h1>
              <p className={style.subtitle}>{project.internal.description}</p>
            </header>
            <section
              dangerouslySetInnerHTML={{ __html: project.markdown.html!! }}
            />
          </Col>
          <Col lg={4}>
            <Sidebar project={project} />
          </Col>
        </Row>
      </Container>
    </Layout>
  )
}

export default ProjectDetailTemplate
