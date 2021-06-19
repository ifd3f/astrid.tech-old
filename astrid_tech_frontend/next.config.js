const { getEnv } = require("./lib/jsenvutil");
const withPlugins = require("next-compose-plugins");


module.exports = withPlugins(
  [
  ],
  {
    trailingSlash: true,
    env: {
      publicRoot: "https://astrid.tech/",
      apiRoot: getEnv("ASTRID_TECH_API_ROOT", "http://localhost:8001"),
    },
    exportPathMap() {
      return {
        "/404": { page: "/404" }
      }
    },
  }
);
