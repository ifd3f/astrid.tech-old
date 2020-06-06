import axios from "axios";
import path from "path";
// import { Post } from './types'

export default {
  entry: path.join(__dirname, "src", "index.tsx"),
  getRoutes: async () => {
    const { data: posts } /* :{ data: Post[] } */ = await axios.get(
      "https://jsonplaceholder.typicode.com/posts"
    );
    return [];
  },
  plugins: [
    "react-static-plugin-typescript",
    [
      require.resolve("react-static-plugin-source-filesystem"),
      {
        location: path.resolve("./src/pages"),
      },
    ],
    require.resolve("react-static-plugin-reach-router"),
    require.resolve("react-static-plugin-sitemap"),
  ],
};
