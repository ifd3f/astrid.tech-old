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

module.exports = {
  images: {
    domains: ["i.ytimg.com"],
  },
  async redirects() {
    return [
      {
        source: "/projects/:slug",
        destination: "/p/:slug",
        permanent: true,
      },
      {
        source: "/blog",
        destination: "/b",
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
      oldBlogRedirect("2020-06-18-frequency-shifting"),
      oldBlogRedirect("2020-07-10-website-release"),
      oldBlogRedirect("2020-07-26-new-gatsby-config"),
      oldBlogRedirect("2020-09-06-malloc"),
      oldBlogRedirect("2020-09-15-latex-note-taking"),
      oldBlogRedirect("2020-10-17-astrid-tech-v1"),
      oldBlogRedirect("2020-11-14-kombucha"),
      oldBlogRedirect("2020-11-22-n-body-collision"),
      oldBlogRedirect("2020-12-20-adding-a-backend"),
    ];
  },
};
