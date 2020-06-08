const path = require(`path`)
const { createFilePath } = require(`gatsby-source-filesystem`)

const createBlogPosts = async ({ graphql, actions }) => {
  const { createPage } = actions

  const BlogPostTemplate = path.resolve(`./src/templates/blog-post.tsx`)
  const result = await graphql(`
    {
      allMarkdownRemark(
        filter: { frontmatter: { type: { eq: "blog" } } }
        sort: { fields: [frontmatter___date], order: DESC }
        limit: 1000
      ) {
        edges {
          node {
            fields {
              slug
            }
            frontmatter {
              title
            }
          }
        }
      }
    }
  `)

  if (result.errors) {
    throw result.errors
  }

  // Create blog posts pages.
  const posts = result.data.allMarkdownRemark.edges

  posts.forEach((post, index) => {
    const previous = index === posts.length - 1 ? null : posts[index + 1].node
    const next = index === 0 ? null : posts[index - 1].node

    createPage({
      path: post.node.fields.slug,
      component: BlogPostTemplate,
      context: {
        slug: post.node.fields.slug,
        previous,
        next,
      },
    })
  })
}

const createProjects = async ({ graphql, actions }) => {
  const { createPage } = actions

  const ProjectDetail = path.resolve(`./src/templates/project-detail.tsx`)
  const result = await graphql(`
    {
      allMarkdownRemark(
        filter: { frontmatter: { type: { eq: "project" } } }
        sort: { fields: [frontmatter___date], order: DESC }
        limit: 1000
      ) {
        edges {
          node {
            fields {
              slug
            }
            frontmatter {
              title
            }
          }
        }
      }
    }
  `)

  if (result.errors) {
    throw result.errors
  }

  const projects = result.data.allMarkdownRemark.edges

  projects.forEach(project => {
    createPage({
      path: project.node.fields.slug,
      component: ProjectDetail,
      context: {
        slug: project.node.fields.slug,
      },
    })
  })
}

exports.createPages = async ({ graphql, actions }) => {
  createBlogPosts({ graphql, actions })
  createProjects({ graphql, actions })
}

exports.onCreateNode = ({ node, actions, getNode }) => {
  const { createNodeField } = actions

  if (node.internal.type === `MarkdownRemark`) {
    const filePath = createFilePath({ node, getNode })
    const type = node.frontmatter.type
    createNodeField({
      name: `slug`,
      node,
      value: "/" + type + filePath,
    })

    if (type == "project") {
      const thumbnailPath = path.resolve(
        path.parse(node.fileAbsolutePath).dir,
        node.frontmatter.thumbnail
      )
      console.log(thumbnailPath)
    }
  }
}
