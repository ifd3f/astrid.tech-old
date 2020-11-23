/**
 * SEO component that queries for data with
 *  Gatsby's useStaticQuery React hook
 *
 * See: https://www.gatsbyjs.org/docs/use-static-query/
 */

import { graphql, useStaticQuery } from "gatsby"
import React, { FC } from "react"
import { Helmet } from "react-helmet"

interface SEOProps {
  description?: string
  lang?: string
  meta?: any[]
  image?: string
  title: string
}

const SEO: FC<SEOProps> = ({
  description = "",
  lang = "en",
  meta = [],
  image,
  title,
}) => {
  const { site } = useStaticQuery(
    graphql`
      query {
        site {
          siteMetadata {
            title
            description
            social {
              twitter
            }
          }
        }
      }
    `
  )

  const metaDescription = description || site.siteMetadata.description

  const metas = [
    {
      name: `description`,
      content: metaDescription,
    },
    {
      property: `og:title`,
      content: title,
    },
    {
      property: `og:description`,
      content: metaDescription,
    },
    {
      property: `og:type`,
      content: `website`,
    },
    {
      name: `twitter:card`,
      content: `summary`,
    },
    {
      name: `twitter:creator`,
      content: site.siteMetadata.social.twitter,
    },
    {
      name: `twitter:title`,
      content: title,
    },
    {
      name: `twitter:description`,
      content: metaDescription,
    },
  ].concat(meta)
  if (image) {
    metas.push({ name: "og:image", content: image })
  }

  return (
    <Helmet
      htmlAttributes={{
        lang,
      }}
      title={title}
      titleTemplate={`%s | ${site.siteMetadata.title}`}
      meta={metas}
    >
      <link
        title="Subscribe to the Blog!"
        rel="alternate"
        type="application/rss+xml"
        href="http://astrid.tech/feed"
      />
      <link href="https://github.com/Plenglin" rel="me authn" />
      <link href="mailto:astrid@astrid.tech" rel="me" />
    </Helmet>
  )
}

export default SEO
