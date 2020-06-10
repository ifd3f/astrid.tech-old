import { Project } from "../types"
import { PageProps, graphql } from "gatsby"
import { FC } from "react"
import Layout from "../components/layout"
import React from "react"

export const pageQuery = graphql`
  query GetProject($id: String!) {
    allProject(filter: { id: { eq: $id } }) {
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

  return (
    <Layout>
      <article>
        <header>
          <h1>{project.title!}</h1>
        </header>
      </article>
    </Layout>
  )
}

export default ProjectDetailTemplate
