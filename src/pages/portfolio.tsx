import { graphql, Link, PageProps } from "gatsby"
import React from "react"
import Layout from "../components/layout"
import SEO from "../components/seo"
import { rhythm } from "../utils/typography"
import { Project } from "../types"
import { ProjectCard } from "../components/project"
import { CardColumns, Container } from "reactstrap"

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
    allProject(sort: { fields: [startDate], order: DESC }) {
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

const ProjectsIndex = ({ data }: PageProps<Data>) => {
  const projects = data.allProject.edges.map(edge => edge.node)

  return (
    <Layout>
      <SEO title="Portfolio" />
      <Container fluid>
        <div className="wide-card-columns">
          <CardColumns>
            {projects.map(project => (
              <ProjectCard project={project} />
            ))}
          </CardColumns>
        </div>
      </Container>
    </Layout>
  )
}

export default ProjectsIndex
