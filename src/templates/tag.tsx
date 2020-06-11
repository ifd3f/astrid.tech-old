import { graphql, PageProps } from "gatsby"
import React, { FC } from "react"
import Layout from "../components/layout"
import { Tag } from "../types/index"
import { TagBadge } from "../components/util"
import SEO from "../components/seo"

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
      <SEO title={tag.name!} />
      <header>
        <h1>
          <TagBadge tag={tag} />
        </h1>
      </header>
    </Layout>
  )
}

export default TagDetailTemplate
