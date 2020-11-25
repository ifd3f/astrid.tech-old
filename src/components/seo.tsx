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
  origin?: string
  canonicalUrl?: string
}

const SEO: FC<SEOProps> = ({
  description = "",
  lang = "en",
  meta = [],
  origin = "http://astrid.tech",
  canonicalUrl,
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
  ].concat(meta)
  if (image) {
    metas.push({ property: "og:image", content: image })
    metas.push({ property: "twitter:image", content: image })
    metas.push({ property: "twitter:card", content: "summary" })
  }

  if (canonicalUrl) {
    metas.push({ property: "og:url", content: canonicalUrl })
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
        title="Blog"
        rel="alternate"
        type="application/rss+xml"
        href={`${location}/feed`}
      />

      {/* Web login */}
      <link href="https://github.com/Plenglin" rel="me authn" />
      <link href="mailto:astrid@astrid.tech" rel="me" />
    </Helmet>
  )
}

export default SEO
