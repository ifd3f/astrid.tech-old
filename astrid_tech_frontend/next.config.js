const { getEnv } = require("./lib/jsenvutil");
const withPlugins = require("next-compose-plugins");
const optimizedImages = require("next-optimized-images");

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

module.exports = withPlugins(
  [
    [
      optimizedImages,
      {
        responsive: {
          adapter: require('responsive-loader/sharp')
        },
        handleImages: ["jpeg", "jpg", "JPG", "png", "svg", "webp", "gif"],
      },
    ],
  ],
  {
    trailingSlash: true,
    env: {
      publicRoot: "https://astrid.tech/",
      apiRoot: getEnv("ASTRID_TECH_API_ROOT", "http://localhost:8001"),
    },
    async redirects() {
      return [
        {
          source: "/blog",
          destination: "/latest",
          permanent: true,
        },
        {
          source: "/projects/hyposcale-cluster/",
          destination: "/projects/plebscale/",
          permanent: true,
        },
        {
          source: "/2020/01/27/kube-update/",
          destination: "/2020/01/27/pi-clustering/",
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
  }
);
