const { getEnv } = require("./lib/jsenvutil");
const withPlugins = require("next-compose-plugins");


module.exports = withPlugins(
  [
  ],
  {
    trailingSlash: true,
    env: {
      publicRoot: "https://astrid.tech/",
      apiRoot: process.env.ASTRID_TECH_API_ROOT
    },
    exportPathMap() {
      return {
        "/404": { page: "/404" }
      }
    },
  }
);
