function oldBlogRedirect(oldSlug, newSlug) {
  const [, year, month, day, slug] = oldSlug.match(
    /(\d{4})-(\d{2})-(\d{2})-(.+)/
  );
  newSlug = newSlug ?? slug;
  return {
    source: `/blog/${year}-${month}-${day}-${slug}`,
    destination: `/${year}/${month}/${day}/${newSlug}`,
    permanent: true,
  };
}

function getAPIRoot() {
  if (process.env.ASTRID_TECH_API_ROOT) {
    return process.env.ASTRID_TECH_API_ROOT;
  } else if (process.env.NODE_ENV == "production") {
    throw new Error(
      "We are in production, but ASTRID_TECH_API_ROOT was not specified!"
    );
  } else {
    const apiRoot = "http://localhost:8001/";
    console.warn("No API root specified, defaulting to", apiRoot);
    return apiRoot;
  }
}

module.exports = {
  images: {
    domains: ["i.ytimg.com"],
  },
  env: {
    publicRoot: "https://astrid.tech/",
    apiRoot: getAPIRoot(),
  },
  async redirects() {
    return [
      {
        source: "/blog",
        destination: "/latest",
        permanent: true,
      },
      {
        source: "/tags/:slug",
        destination: "/t/:slug",
        permanent: true,
      },
      {
        source: "/tags",
        destination: "/t",
        permanent: true,
      },
      oldBlogRedirect("2020-02-06-cal-poly-pride-center", "cp-pride-center"),
      oldBlogRedirect("2020-06-07-hello-world"),
      oldBlogRedirect("2020-06-08-cs-major"),
      oldBlogRedirect("2020-06-18-frequency-shifting", "freq-shift"),
      oldBlogRedirect("2020-07-10-website-release", "site-release"),
      oldBlogRedirect("2020-07-26-new-gatsby-config", "gatsby-backend"),
      oldBlogRedirect("2020-09-06-malloc"),
      oldBlogRedirect("2020-09-15-latex-note-taking", "latex"),
      oldBlogRedirect("2020-10-17-astrid-tech-v1"),
      oldBlogRedirect("2020-11-14-kombucha"),
      oldBlogRedirect("2020-11-22-n-body-collision"),
      oldBlogRedirect("2020-12-20-adding-a-backend", "backend"),
    ];
  },
};
