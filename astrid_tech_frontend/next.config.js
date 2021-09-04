const withPlugins = require("next-compose-plugins");

if (!process.env.ASTRID_TECH_API_ROOT) {
  throw new Error("Please specify an ASTRID_TECH_API_ROOT");
}

module.exports = withPlugins([], {
  trailingSlash: true,
  eslint: {
    ignoreDuringBuilds: true, // TODO get rid of this
  },
  env: {
    publicRoot: "https://astrid.tech/",
    apiRoot: process.env.ASTRID_TECH_API_ROOT,
  },
  exportPathMap() {
    return {
      "/404": { page: "/404" },
    };
  },
});
