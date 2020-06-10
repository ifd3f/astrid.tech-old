const axios = require("axios")
const path = require(`path`)
const fs = require("fs")
const uuid = require("uuid")
const { createFilePath } = require(`gatsby-source-filesystem`)
const crypto = require("crypto")
const rimraf = require("rimraf")
const mkdirp = require("mkdirp")
const md5 = require("js-md5")
const yaml = require("js-yaml")

const SLUG_OVERRIDE = {
  "c++": "cpp",
  "c#": "csharp",
  "f#": "fsharp",
  "objective-c++": "objective-cpp",
}

const getTagSlug = name => {
  const lower = name.toLowerCase()
  return SLUG_OVERRIDE[lower] || lower.replace(" ", "-")
}

const setContentDigest = node => {
  // Get content digest of node. (Required field)
  const contentDigest = crypto
    .createHash(`md5`)
    .update(JSON.stringify(node))
    .digest(`hex`)
  // add it to userNode
  node.internal.contentDigest = contentDigest
}

const buildTagNode = ({ name, slug, color, textColor }) => {
  const node = {
    parent: `__SOURCE__`,
    internal: {
      type: `Tag`,
    },
    children: [],

    id: uuid.v4(),
    name,
    slug,
    color,
    textColor,
  }

  // Get content digest of node. (Required field)
  const contentDigest = crypto
    .createHash(`md5`)
    .update(JSON.stringify(node))
    .digest(`hex`)
  // add it to userNode
  node.internal.contentDigest = contentDigest

  return node
}

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

exports.onPreBootstrap = () => {
  rimraf.sync(path.resolve(`${__dirname}/static/generated`))
}

const sourceLangTagNodes = async ({ actions }) => {
  const { createNode } = actions

  const res = await axios.get(
    "https://raw.githubusercontent.com/github/linguist/master/lib/linguist/languages.yml"
  )

  const tagTable = {}
  const langs = yaml.load(res.data)
  for (let key in langs) {
    if (!langs.hasOwnProperty(key)) {
      continue
    }

    const lang = langs[key]
    if (!lang.color) {
      continue
    }

    // Get RGB components
    const [, r, g, b] = lang.color
      .match(/#([0-9a-f]{2})([0-9a-f]{2})([0-9a-f]{2})/i)
      .map(x => new Number("0x" + x))

    // Taken from https://stackoverflow.com/a/3943023/12947037
    const textColor =
      r * 0.299 + g * 0.587 + b * 0.114 > 186 ? "#000000" : "#ffffff"

    const node = buildTagNode({
      name: key,
      slug: getTagSlug(key),
      color: lang.color,
      textColor,
    })
    tagTable[node.slug] = node.id
    // Create node with the gatsby createNode() API
    createNode(node)
  }

  return tagTable
}

const sourceWorkExperienceNodes = async ({ graphql, actions }) => {
  const { createNode } = actions
  const rawQueryResult = await graphql(`
    {
      allWorkExperienceYaml {
        edges {
          node {
            parent {
              ... on File {
                name
              }
            }
            organization
            position
            location
            startDate
            endDate

            summary
            website

            highlights
            tags
          }
        }
      }
    }
  `)

  rawQueryResult.edges.forEach(edge => {
    const rawNode = edge.node

    const node = {
      parent: `__SOURCE__`,
      internal: {
        type: `WorkExperience`,
      },
      id: uuid.v4(),
      children: [],

      slug: "/experience/" + rawNode.parent.name,
      organization: rawNode.organization,
      position: rawNode.position,
      location: rawNode.location,
      startDate: rawNode.startDate,
      endDate: rawNode.endDate,

      summary: rawNode.summary,
      website: rawNode.website,

      highlights: rawNode.highlights,
      ["tags__NODE"]: rawNode.tags,
    }

    setContentDigest(node)

    createNode(
      buildTagNode({
        name: `${node.position} at ${node.organization}`,
        slug: node.slug,
        color: "#169bf4",
        textColor: "#16f4de",
      })
    )

    createNode(node)
  })
}

const sourceProjectNodes = async ({ graphql, actions }) => {
  const { createNode } = actions

  const rawMarkdownQuery = await graphql(`
    {
      allMarkdownRemark(
        filter: { frontmatter: { type: { eq: "project" } } }
        sort: { fields: [frontmatter___date], order: DESC }
        limit: 1000
      ) {
        edges {
          node {
            markdownId: id
            fields {
              slug
            }
            frontmatter {
              title
              tags
              status
              description
              startDate
              endDate
              url
              source
              thumbnail
            }
          }
        }
      }
    }
  `)

  rawMarkdownQuery.data.allMarkdownRemark.edges.forEach(edge => {
    const project = edge.node

    const projectNode = {
      parent: `__SOURCE__`,
      internal: {
        type: `WorkExperience`,
      },
      id: uuid.v4(),
      children: [],

      slug: project.fields.slug,
      title: project.frontmatter.title,
      tags: project.frontmatter.tags,
      status: project.frontmatter.status,
      description: project.frontmatter.description,
      startDate: project.frontmatter.startDate,
      endDate: project.frontmatter.endDate,
      url: project.frontmatter.url,
      source: project.frontmatter.source,
      thumbnail: project.frontmatter.thumbnail,
      ["markdown__NODE"]: project.markdownId,
    }

    setContentDigest(projectNode)
    createNode(projectNode)

    const tagNode = buildTagNode({
      name: project.frontmatter.title,
      slug: project.fields.slug,
      color: "#e81272",
      textColor: "#e0c23e",
    })

    createNode(tagNode)
  })
}

const createProjectPages = async () => {
  const ProjectDetail = path.resolve(`./src/templates/project-detail.tsx`)

  createPage({
    path: project.fields.slug,
    component: ProjectDetail,
    context: {
      slug: project.fields.slug,
    },
  })
}

exports.sourceNodes = async ({ graphql, actions }) => {
  const tagTable = sourceLangTagNodes({ graphql, actions })
  sourceWorkExperienceNodes({ tagTable, graphql, actions })
  sourceProjectNodes({ tagTable, graphql, actions })
}

exports.createPages = async ({ graphql, actions }) => {
  //createBlogPosts({ graphql, actions })
  createProjectPages({ graphql, actions })
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

    let thumbnailPublicPath = null
    if (type == "project") {
      const thumbnailLoc = node.frontmatter.thumbnail
      if (thumbnailLoc) {
        const markdownPath = path.parse(node.fileAbsolutePath)
        const absThumbnailPath = path.resolve(
          markdownPath.dir + "/" + thumbnailLoc
        )
        const pathHash = md5(absThumbnailPath)

        const parsedThumbnailPath = path.parse(absThumbnailPath)

        thumbnailPublicPath = `/generated/projects/${pathHash}-${parsedThumbnailPath.name}${parsedThumbnailPath.ext}`

        const copiedPath = path.resolve(
          `${__dirname}/static/${thumbnailPublicPath}`
        )

        mkdirp.sync(path.parse(copiedPath).dir)
        fs.copyFileSync(absThumbnailPath, copiedPath)
      }
    }
    createNodeField({
      name: "thumbnailPublicPath",
      node,
      value: thumbnailPublicPath,
    })
  }
}
