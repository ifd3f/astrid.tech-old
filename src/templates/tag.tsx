import { graphql, PageProps } from "gatsby"
import React, { FC } from "react"
import Layout from "../components/layout"
import { Tag } from "../types/index"

export const pageQuery = graphql`
  query GetTag($id: String!) {
    allTag(filter: { id: { eq: $id } }) {
      edges {
        node {
          slug
          name
          color
          textColor
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

  return (
    <Layout>
      <article>
        <header>
          <h1>{tag.name!}</h1>
        </header>
      </article>
    </Layout>
  )
}

export default TagDetailTemplate
