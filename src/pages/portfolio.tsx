import { graphql, PageProps } from "gatsby"
import React, { useState, FC } from "react"
import { CardColumns, Container, Row, Col } from "reactstrap"
import Layout from "../components/layout"
import { ProjectCard } from "../components/project"
import SEO from "../components/seo"
import { Project } from "../types"
import Masonry from "react-masonry-component"
import { Timeline, IntervalNode } from "../components/timeline"
type Data = {
  site: {
    siteMetadata: {
      title: string
    }
  }
  allProject: {
    edges: [
      {
        node: Project
      }
    ]
  }
}

export const pageQuery = graphql`
  {
    site {
      siteMetadata {
        title
      }
    }
    allProject(sort: { fields: [endDate], order: DESC }) {
      edges {
        node {
          slug
          thumbnailPublicPath
          startDate(formatString: "YYYY-MM")
          endDate(formatString: "YYYY-MM")
          title
          description
          status
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
  }
`

function getMonths(dateString: string | null): number {
  const date = dateString ? new Date(dateString) : new Date()
  return date.getFullYear() * 12 + date.getMonth()
}

type PortfolioTimelineBlockProps = {
  hover: boolean
  project: Project
  onEnter?: (project: Project) => void
  onLeave?: (project: Project) => void
}

const PortfolioTimelineBlock: FC<PortfolioTimelineBlockProps> = ({
  hover,
  project,
  onEnter: _onEnter,
  onLeave: _onLeave,
}) => {
  const color = hover ? "#444444" : "#555555"

  const onEnter = () => _onEnter && _onEnter(project)
  const onLeave = () => _onLeave && _onLeave(project)

  return (
    <div
      style={{
        width: "100%",
        height: "100%",
        backgroundColor: color,
      }}
      onMouseEnter={onEnter}
      onMouseLeave={onLeave}
    >
      {" "}
    </div>
  )
}

const ProjectsIndex = ({ data }: PageProps<Data>) => {
  const projects = data.allProject.edges.map(edge => edge.node)

  const [hoveredProject, setHoveredProject] = useState<string | null>(null)

  const hoverStateMap = new Map<
    string,
    { get: () => boolean; set: (state: boolean) => void }
  >()

  const onEnter = (project: Project) => {
    setHoveredProject(project.slug)
  }

  const onLeave = (project: Project) => {
    if (hoveredProject == project.slug) {
      setHoveredProject(null)
    }
  }

  const intervals: IntervalNode[] = projects.map(proj => ({
    start: -getMonths(proj.startDate),
    end: -getMonths(proj.endDate),
    content: ({ width, height }) => (
      <div
        style={{
          width: width,
          height: height,
        }}
      >
        <PortfolioTimelineBlock
          hover={hoveredProject == proj.slug}
          project={proj}
          onEnter={onEnter}
          onLeave={onLeave}
        />
      </div>
    ),
  }))

  const cards = projects.map(project => (
    <Col xs={3}>
      <ProjectCard
        project={project}
        hovered={project.slug == hoveredProject}
        onMouseEnter={onEnter}
        onMouseLeave={onLeave}
      />
    </Col>
  ))

  const [timelineColRef, setTimelineColRef] = useState<HTMLElement | null>(null)

  const timelineCol = (
    <div
      style={{
        height: "90%",
        width: 200,
        position: "fixed",
        top: 70,
        right: 0,
      }}
    >
      <div style={{ width: "100%", height: "100%" }} ref={setTimelineColRef}>
        <Timeline
          intervals={intervals}
          width={timelineColRef?.clientWidth ?? 100}
          height={timelineColRef?.clientHeight ?? 100}
        />
      </div>
    </div>
  )

  return (
    <Layout>
      <SEO title="Portfolio" />
      <Container fluid>
        <Row>
          <Col lg={9}>
            <Masonry>{cards}</Masonry>
          </Col>
        </Row>
      </Container>
      {timelineCol}
    </Layout>
  )
}

export default ProjectsIndex
