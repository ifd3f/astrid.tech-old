import _ from "lodash";
import {
  GetServerSideProps,
  GetStaticProps,
  InferGetServerSidePropsType,
  InferGetStaticPropsType,
} from "next";
import { FC } from "react";
import BlogPostPage, { ClientBlogPost } from "../components/blog/blog";
import {
  getBlogPost,
  getBlogPostSlugs,
  getPathFromEntry,
  Path,
  resolveBlogPost,
} from "../lib/cache";
import { renderContentToHTML, renderMarkdown } from "../lib/markdown";
import { wrappedStaticPaths } from "../lib/pathcache";
import { BlogPost, convertBlogPostToObjectDate } from "../types/types";

export const getServerSideProps: GetServerSideProps = async ({
  params,
  res,
}) => {
  const path = params!!.path!! as string[];

  // // Check if this is a permashortlink query
  // if (path.length == 1) {
  //   const [psl] = path;
  //   if (isNaN(parseInt(psl))) {
  //     // TODO (perma)shortlink handling
  //   }
  // }

  // // Check if this is a year/month/day querry
  // if (path.length <= 3) {
  //   // TODO timespan page
  //   return {
  //     props: {
  //       pageType: 'entries',
  //       posts: [],  // TODO
  //     },
  //   };
  // }

  // Resolve as if this is a blog post

  res.setHeader(
    "Cache-Control",
    "public, s-maxage=10, stale-while-revalidate=59"
  );

  const [year, month, day, ordinal, slugName] = path as string[];

  const posts = await resolveBlogPost(year, month, day, ordinal);
  console.debug("Resolved blog posts", posts);

  // We only found one blog post
  if (posts.length == 1) {
    const [post] = posts;
    const segments = getPathFromEntry(post);
    console.log("There is a single blog post", segments, post);

    // If this is not the canonical URL, redirect to the canonical URL
    if (!_.isEqual(segments, path)) {
      const destination = "/" + segments.join("/");
      console.log("Redirecting user to canonical URL", destination);

      return {
        redirect: {
          permanent: false,
          destination,
        },
      };
    }

    // Render the content
    console.log("Rendering content");

    const start = new Date().getTime();
    const fullPost = await getBlogPost(year, month, day, ordinal);
    const htmlContent = fullPost.content
      ? await renderContentToHTML(fullPost.content_type, fullPost.content)
      : null; // TODO
    console.log(new Date().getTime() - start);

    return {
      props: {
        pageType: "post",
        post: {
          title: fullPost.title,
          htmlContent,
          path,
          tags: fullPost.tags,
          description: fullPost.description,
        } as ClientBlogPost,
      },
    };
  }

  return {
    props: {
      pageType: "posts",
      posts,
    },
  };
};

const Post: FC<InferGetServerSidePropsType<typeof getServerSideProps>> = ({
  pageType,
  post,
  posts,
}) => {
  if (pageType == "post") {
    return <BlogPostPage post={post} />;
  }
  return <p>TODO multiple Posts</p>; // TODO
};

export default Post;
