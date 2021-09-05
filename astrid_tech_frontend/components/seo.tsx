import Head from "next/head";
import { FC } from "react";

interface SEOProps {
  description?: string | null;
  lang?: string;
  meta?: any[];
  image?: string;
  title: string;
  origin?: string;
  canonicalUrl?: string;
}

const SEO: FC<SEOProps> = ({
  description,
  lang = "en",
  meta = [],
  canonicalUrl,
  image,
  title,
}) => {
  const metas = [
    {
      name: "viewport",
      content: "width=device-width,initial-scale=1.0",
    },
    {
      name: `twitter:card`,
      content: "summary",
    },
    {
      name: `twitter:creator`,
      content: "astralbijection",
    },
    {
      name: `twitter:title`,
      content: title,
    },
    {
      property: `og:title`,
      content: title,
    },
    {
      property: `og:type`,
      content: `website`,
    },
    {
      name: "robots",
      content: "follow,index",
    },
  ].concat(meta);

  if (description) {
    ["description", "twitter:description", "og:description"].forEach((name) => {
      metas.push({ name, content: description });
    });
  }

  if (image) {
    metas.push({ property: "og:image", content: image });
    metas.push({ property: "twitter:image", content: image });
    metas.push({ property: "twitter:card", content: "summary" });
  }

  if (canonicalUrl) {
    metas.push({ property: "og:url", content: canonicalUrl });
  }

  return (
    <Head>
      <title>{title} | astrid.tech</title>

      {metas.map((props) => (
        <meta key={props.property ?? props.name} {...props} />
      ))}

      <link
        title="astrid.tech RSS"
        rel="alternate"
        type="application/rss+xml"
        href={`${process.env.publicRoot}/rss.xml`}
      />
      <link
        title="astrid.tech Atom"
        rel="alternate"
        type="application/atom+xml"
        href={`${process.env.publicRoot}/atom.xml`}
      />
      <link
        title="astrid.tech JSON feed"
        rel="alternate"
        type="application/feed+json"
        href={`${process.env.publicRoot}/feed.json`}
      />

      {/* IndieAuth */}
      <link rel="authorization_endpoint" href="https://indieauth.com/auth" />

      {/* Webmention */}
      <link
        rel="webmention"
        href="https://api.astrid.tech/api/webmention/receive"
      />
    </Head>
  );
};

export default SEO;
