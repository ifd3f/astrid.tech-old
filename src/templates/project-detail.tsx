import { Project } from "../types"
import { PageProps, graphql } from "gatsby"
import { FC } from "react"
import Layout from "../components/layout/layout"
import React from "react"
import SEO from "../components/seo"
import { Container } from "reactstrap"
import { TagList } from "../components/tag"
import { StatusBadge } from "../components/project"
import style from "./project-detail.module.scss"

export const pageQuery = graphql`
  query GetProject($id: String!) {
    allProject(filter: { id: { eq: $id } }) {
      edges {
        node {
          slug
          thumbnailPublicPath
          startDate(formatString: "MMMM YYYY")
          endDate(formatString: "MMMM YYYY")
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

const ProjectDetailTemplate: FC<PageProps<Data, Context>> = ({ data }) => {
  const project = data.allProject.edges[0].node

  const date = project.endDate
    ? `${project.startDate} to ${project.endDate}`
    : project.startDate

  return (
    <Layout>
      <SEO title={project.title!} />
      <Container className={style.projectDetailContainer}>
        <article>
          <header>
            <h1>
              {project.title!} <StatusBadge status={project.status} />
            </h1>
            <p className={style.date}>{date}</p>
            <p className={style.subtitle}>{project.description}</p>
            <TagList tags={project.tags.map(x => x.tag!)} />
          </header>
          <section
            dangerouslySetInnerHTML={{ __html: project.markdown!!.html!! }}
          />
        </article>
      </Container>
    </Layout>
  )
}

export default ProjectDetailTemplate
