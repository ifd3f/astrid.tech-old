import { GatsbyNode } from "gatsby"
import { resolve } from "path"

export const createPages: GatsbyNode["createPages"] = async ({
  actions,
  graphql,
}) => {
  const { createPage } = actions

  console.log("privyet")
}
