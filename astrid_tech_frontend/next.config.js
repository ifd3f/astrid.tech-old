const { getEnv } = require("./lib/jsenvutil");
const withPlugins = require("next-compose-plugins");
const optimizedImages = require("next-optimized-images");


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
    exportPathMap() {
      return {
        "/404": { page: "/404" }
      }
    },
  }
);
