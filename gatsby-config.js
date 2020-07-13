require("source-map-support").install()
require("ts-node").register()

module.exports = {
  siteMetadata: {
    title: `astrid.tech`,
    version: "0.1.0",
    author: {
      name: `Astrid Yu`,
      summary: `who likes to engineer awesome things`,
      pronouns: {
        subj: `she`,
        obj: `her`,
        pos: `hers`,
        posAdj: `her`,
        reflex: `herself`,
      },
    },
    description: `Astrid Yu's blog and portfolio`,
    siteUrl: `https://astrid.tech/`,
    social: {
      twitter: `none`,
      github: `Plenglin`,
    },
  },
  plugins: [
    {
      resolve: `gatsby-source-filesystem`,
      options: {
        path: `${__dirname}/content/blog`,
        name: `blog`,
      },
    },
    {
      resolve: `gatsby-source-filesystem`,
      options: {
        path: `${__dirname}/content/projects`,
        name: `projects`,
      },
    },
    {
      resolve: `gatsby-source-filesystem`,
      options: {
        path: `${__dirname}/content/work`,
        name: `work`,
      },
    },
    {
      resolve: `gatsby-source-filesystem`,
      options: {
        path: `${__dirname}/content/tags`,
        name: `tags`,
      },
    },
    {
      resolve: `gatsby-source-filesystem`,
      options: {
        path: `${__dirname}/content/assets`,
        name: `assets`,
      },
    },
    {
      resolve: `gatsby-source-filesystem`,
      options: {
        path: `${__dirname}/assets`,
        name: `assets`,
      },
    },
    {
      resolve: `gatsby-source-filesystem`,
      options: {
        path: `${__dirname}/content/misc`,
        name: `misc-data`,
      },
    },
    `gatsby-transformer-ipynb`,
    `gatsby-transformer-yaml`,
    {
      resolve: `gatsby-transformer-remark`,
      options: {
        plugins: [
          {
            resolve: `gatsby-remark-images`,
            options: {
              maxWidth: 590,
            },
          },
          {
            resolve: `gatsby-remark-responsive-iframe`,
            options: {
              wrapperStyle: `margin-bottom: 1.0725rem`,
            },
          },
          "gatsby-remark-katex",
          "gatsby-remark-graphviz",
          `gatsby-remark-prismjs`,
          `gatsby-remark-copy-linked-files`,
          `gatsby-remark-smartypants`,
        ],
      },
    },
    `gatsby-transformer-sharp`,
    `gatsby-plugin-sharp`,
    {
      resolve: `gatsby-plugin-google-analytics`,
      options: {
        //trackingId: `UA-171109022-1`,
      },
    },
    {
      resolve: `gatsby-plugin-build-date`,
      options: {
        formatAsDateString: true,
        formatting: {
          format: "HH:MM:SS dddd D MMMM YYYY",
          utc: true,
        },
      },
    },
    `gatsby-plugin-feed`,
    {
      resolve: `gatsby-plugin-manifest`,
      options: {
        name: `Gatsby Starter Blog`,
        short_name: `GatsbyJS`,
        start_url: `/`,
        background_color: `#ffffff`,
        theme_color: `#663399`,
        display: `minimal-ui`,
        icon: `assets/astrid-tech-icon.png`,
      },
    },
    `gatsby-plugin-react-helmet`,
    "gatsby-plugin-sass",

    "gatsby-astrid-plugin-blog",
    "gatsby-astrid-plugin-tagging",

    "gatsby-astrid-source-lang-tags",
    "gatsby-astrid-transformer-user-tags",
    "gatsby-astrid-transformer-skills",

    "gatsby-astrid-transformer-work",
    "gatsby-astrid-transformer-education",

    "gatsby-astrid-transformer-notebook-markdown",
    "gatsby-astrid-transformer-markdown-post",
    "gatsby-astrid-transformer-project",
  ],
}
