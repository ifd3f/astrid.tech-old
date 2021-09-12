const withPlugins = require("next-compose-plugins");

if (!process.env.ASTRID_TECH_API_ROOT) {
  throw new Error("Please specify ASTRID_TECH_API_ROOT");
}

if (!process.env.SITE_ROOT) {
  throw new Error("Please specify SITE_ROOT");
}

if (!process.env.SHORT_ROOT) {
  throw new Error("Please specify SHORT_ROOT");
}

module.exports = withPlugins([], {
  trailingSlash: true,
  eslint: {
    ignoreDuringBuilds: false,
  },
  env: {
    publicRoot: process.env.SITE_ROOT,
    apiRoot: process.env.ASTRID_TECH_API_ROOT,
    shortRoot: process.env.SHORT_ROOT
  },
  exportPathMap() {
    return {
      "/404": { page: "/404" },
    };
  },
});
