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
  const preprocessed = `astrid.tech-tag-${slug}`
  return md5(preprocessed)
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
    parent: yamlNode.id,
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
    parent: markdownNode.id,
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

    markdown___NODE: markdownNode.id,
    tags: createLinkedTagList(markdownNode.frontmatter.tags),
  }
  setContentDigest(projectNode)
  createNode(projectNode)
  createParentChildLink({ parent: markdownNode, child: projectNode })
}

const createMarkdownBlogPostNode = (actions, markdownNode) => {
  const { createNode, createParentChildLink } = actions
  const postNode = {
    parent: markdownNode.id,
    internal: {
      type: `BlogPost`,
    },
    id: uuid.v4(),
    children: [],

    slug: markdownNode.fields.slug,
    title: markdownNode.frontmatter.title,
    date: markdownNode.frontmatter.date,
    description: markdownNode.frontmatter.description,

    contentType: "markdown",
    markdown___NODE: markdownNode.id,
    mdx___NODE: null,
    jupyter___NODE: null,

    tags: createLinkedTagList(markdownNode.frontmatter.tags),
  }

  setContentDigest(postNode)
  createNode(postNode)
  createParentChildLink({ parent: markdownNode, child: postNode })
}

const createBlogPosts = async ({ graphql, actions }) => {
  const { createPage } = actions

  const BlogPostTemplate = path.resolve(`./src/templates/blog-post.tsx`)
  const result = await graphql(`
    {
      allBlogPost(sort: { fields: date, order: DESC }) {
        edges {
          node {
            title
            id
            slug
          }
        }
      }
    }
  `)

  if (result.errors) {
    throw result.errors
  }

  // Create blog posts pages.
  const posts = result.data.allBlogPost.edges
  posts.forEach((edge, index) => {
    const post = edge.node

    const previous = index === posts.length - 1 ? null : posts[index + 1].node
    const next = index === 0 ? null : posts[index - 1].node

    createPage({
      path: post.slug,
      component: BlogPostTemplate,
      context: {
        id: post.id,
        previous,
        next,
      },
    })
  })
}

const createProjectPages = async ({ graphql, actions }) => {
  const { createPage } = actions
  const ProjectDetailTemplate = path.resolve(
    `./src/templates/project-detail.tsx`
  )
  const result = await graphql(`
    {
      allProject(sort: { fields: endDate, order: DESC }) {
        edges {
          node {
            title
            id
            slug
          }
        }
      }
    }
  `)
  if (result.errors) {
    throw result.errors
  }

  result.data.allProject.edges.forEach(({ node: project }) => {
    createPage({
      path: project.slug,
      component: ProjectDetailTemplate,
      context: {
        id: project.id,
      },
    })
  })
}

const createTagPages = async ({ graphql, actions }) => {
  const { createPage } = actions
  const TagTemplate = path.resolve(`./src/templates/tag.tsx`)
  const result = await graphql(`
    {
      allTag {
        edges {
          node {
            id
            slug
          }
        }
      }
    }
  `)
  if (result.errors) {
    throw result.errors
  }

  result.data.allTag.edges.forEach(({ node: tag }) => {
    createPage({
      path: "/tag/" + tag.slug,
      component: TagTemplate,
      context: {
        id: tag.id,
      },
    })
  })
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
      allBlogPost {
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

  ;[
    result.data.allWorkExperience.edges,
    result.data.allProject.edges,
    result.data.allBlogPost.edges,
  ].forEach(edges => {
    edges.forEach(edge => {
      fillDefaultTags(actions, edge.node.tags)
    })
  })

  createBlogPosts({ graphql, actions })
  createProjectPages({ graphql, actions })
  createTagPages({ graphql, actions })
}

exports.onCreateNode = ({ node, actions, getNode }) => {
  const { createNode, createParentChildLink, createNodeField } = actions

  switch (node.internal.type) {
    case `MarkdownRemark`: {
      const filePath = createFilePath({ node, getNode })
      const type = node.frontmatter.type
      createNodeField({
        name: `slug`,
        node,
        value: "/" + type + filePath,
      })

      switch (type) {
        case "project":
          createProjectNode(actions, node)
          break
        case "blog":
          createMarkdownBlogPostNode(actions, node)
          break
      }
      break
    }

    case "WorkExperienceYaml": {
      createWorkExperienceNode(actions, node)
      break
    }

    case "Project": {
      const tagNode = buildTagNode({
        name: node.title,
        slug: node.slug,
        color: "#e81272",
        textColor: "#e0c23e",
      })
      createNode(tagNode)
      createParentChildLink({ parent: node, child: tagNode })
    }
  }
}
