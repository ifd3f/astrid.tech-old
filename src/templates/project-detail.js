import React from "react"
import { Link, graphql } from "gatsby"

import Bio from "../components/bio"
import Layout from "../components/layout"
import SEO from "../components/seo"
import { rhythm, scale } from "../utils/typography"

const ProjectDetail = ({ data, pageContext, location }) => {
  return data.name
}

export default ProjectDetail
