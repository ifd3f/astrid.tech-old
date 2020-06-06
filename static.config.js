import path from "path";
// import { Post } from './types'

export default {
  entry: path.join(__dirname, "src", "index.tsx"),
  plugins: [
    "react-static-plugin-typescript",
    [
      require.resolve("react-static-plugin-source-filesystem"),
      {
        location: path.resolve("./src/pages"),
      },
    ],
    [
      require.resolve("react-static-plugin-source-filesystem"),
      {
        location: path.resolve("./assets"),
      },
    ],
    [
      "react-static-plugin-file-watch-reload",
      {
        // example configuration
        paths: ["src/**/*"],
      },
    ],
    require.resolve("react-static-plugin-reach-router"),
    require.resolve("react-static-plugin-sitemap"),
  ],
};
