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

const getTagId = slug => {
  return md5(`astrid.tech-tag-${slug}`)
}

const createLinkedTagList = slugs =>
  slugs.map(slug => ({
    slug: slug,
    tag___NODE: getTagId(slug),
  }))

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

    id: getTagId(slug),
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

const createWorkExperienceNode = async (actions, yamlNode) => {
  const { createNode, createParentChildLink } = actions
  const workNode = {
    parent: `__SOURCE__`,
    internal: {
      type: `WorkExperience`,
    },
    id: uuid.v4(),
    children: [],

    slug: "/work/" + yamlNode.slug,
    organization: yamlNode.organization,
    position: yamlNode.position,
    location: yamlNode.location,
    startDate: yamlNode.startDate,
    endDate: yamlNode.endDate,

    summary: yamlNode.summary,
    website: yamlNode.website,

    highlights: yamlNode.highlights,

    tags: createLinkedTagList(yamlNode.tags),
  }
  setContentDigest(workNode)
  createNode(workNode)
  createParentChildLink({ parent: yamlNode, child: workNode })

  const tagNode = buildTagNode({
    name: `${workNode.organization}`,
    slug: workNode.slug,
    color: "#169bf4",
    textColor: "#16f4de",
  })
  createNode(tagNode)
  createParentChildLink({ parent: workNode, child: tagNode })
}

const createProjectNode = (actions, markdownNode) => {
  const { createNode, createParentChildLink } = actions
  const thumbnailLoc = markdownNode.frontmatter.thumbnail
  let thumbnailPublicPath = null
  if (thumbnailLoc) {
    const markdownPath = path.parse(markdownNode.fileAbsolutePath)
    const absThumbnailPath = path.resolve(markdownPath.dir + "/" + thumbnailLoc)
    const pathHash = md5(absThumbnailPath)

    const parsedThumbnailPath = path.parse(absThumbnailPath)

    thumbnailPublicPath = `/generated/projects/${pathHash}-${parsedThumbnailPath.name}${parsedThumbnailPath.ext}`

    const copiedPath = path.resolve(
      `${__dirname}/static/${thumbnailPublicPath}`
    )

    mkdirp.sync(path.parse(copiedPath).dir)
    fs.copyFileSync(absThumbnailPath, copiedPath)
  }

  const projectNode = {
    parent: `__SOURCE__`,
    internal: {
      type: `Project`,
    },
    id: uuid.v4(),
    children: [],

    slug: markdownNode.fields.slug,
    title: markdownNode.frontmatter.title,
    status: markdownNode.frontmatter.status,
    description: markdownNode.frontmatter.description,
    startDate: markdownNode.frontmatter.startDate,
    endDate: markdownNode.frontmatter.endDate,
    url: markdownNode.frontmatter.url,
    source: markdownNode.frontmatter.source,
    thumbnailPublicPath: thumbnailPublicPath,

    tags: createLinkedTagList(markdownNode.frontmatter.tags),
  }
  setContentDigest(projectNode)
  createNode(projectNode)
  createParentChildLink({ parent: markdownNode, child: projectNode })

  const tagNode = buildTagNode({
    name: projectNode.title,
    slug: projectNode.slug,
    color: "#e81272",
    textColor: "#e0c23e",
  })
  createNode(tagNode)
  createParentChildLink({ parent: projectNode, child: tagNode })
}

const createProjectPages = async () => {
  const ProjectDetail = path.resolve(`./src/templates/project-detail.tsx`)
}

const createWorkPages = async (tagTable, { graphql, actions }) => {
  const { createNodeField } = actions
  // const result = await graphql(`
  //   {
  //     allWorkExperience {
  //       edges {
  //         node {
  //           tags
  //         }
  //       }
  //     }
  //   }
  // `)
  // if (result.errors) {
  //   throw result.errors
  // }
}

const fillDefaultTags = (actions, tags) => {
  const { createNode } = actions
  tags.forEach(({ slug, tag: linkedNode }) => {
    if (linkedNode) {
      return
    }

    createNode(
      buildTagNode({
        name: slug,
        slug: slug,
        color: "#313549",
        textColor: "#ffffff",
      })
    )
  })
}

exports.sourceNodes = async ({ actions }) => {
  await sourceLangTagNodes({ actions })
}

exports.createPages = async ({ graphql, actions }) => {
  const result = await graphql(`
    {
      allWorkExperience {
        edges {
          node {
            tags {
              slug
              tag {
                id
              }
            }
          }
        }
      }
      allProject {
        edges {
          node {
            tags {
              tag {
                id
              }
              slug
            }
          }
        }
      }
    }
  `)
  if (result.errors) {
    throw result.errors
  }

  result.data.allWorkExperience.edges.forEach(edge => {
    fillDefaultTags(actions, edge.node.tags)
  })

  result.data.allProject.edges.forEach(edge => {
    fillDefaultTags(actions, edge.node.tags)
  })

  //createBlogPosts({ graphql, actions })
  //createWorkPages(tagTable, { graphql, actions })
}

exports.onCreateNode = ({ node, actions, getNode }) => {
  const { createNodeField } = actions

  switch (node.internal.type) {
    case `MarkdownRemark`: {
      const filePath = createFilePath({ node, getNode })
      const type = node.frontmatter.type
      createNodeField({
        name: `slug`,
        node,
        value: "/" + type + filePath,
      })

      if (type == "project") {
        createProjectNode(actions, node)
      }
      break
    }

    case "WorkExperienceYaml": {
      createWorkExperienceNode(actions, node)
      break
    }
  }
}
