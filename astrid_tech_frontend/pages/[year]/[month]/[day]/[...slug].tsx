import {
  GetServerSideProps,
  GetStaticProps,
  InferGetServerSidePropsType,
  InferGetStaticPropsType,
} from "next";
import { FC } from "react";
import BlogPostPage, { ClientBlogPost } from "../../../../components/blog/blog";
import {
  getBlogPost,
  getBlogPostSlugs,
  getPathFromEntry,
  Path,
  resolveBlogPost,
} from "../../../../lib/cache";
import { renderMarkdown } from "../../../../lib/markdown";
import { wrappedStaticPaths } from "../../../../lib/pathcache";
import { BlogPost, convertBlogPostToObjectDate } from "../../../../types/types";

export const getServerSideProps: GetServerSideProps = async ({ params }) => {
  const path = params!! as Path;
  const {
    year,
    month,
    day,
    slug: [ordinal, slugName],
  } = path;

  const posts = await resolveBlogPost(year, month, day, ordinal);
  console.debug("Resolved blog posts", posts);

  if (posts.length == 1) {
    const [post] = posts;
    console.log("There is a single blog post", post);
    if (post.slug_name != slugName) {
      const { year, month, day, slug } = getPathFromEntry(post);
      const segments = [year, month, day].concat(slug);
      const destination = "/" + segments.join("/");
      console.log("Redirecting user to canonical URL", destination);

      return {
        redirect: {
          permanent: false,
          destination,
        },
      };
    }
    console.log("Redirecting user to canonical URL", destination);

    const content = ""; // TODO

    return {
      props: {
        exact: true,
        post: {
          title: post.title,
          contentHtml: content,
          path,
          tags: post.tags,
          description: post.description,
        } as ClientBlogPost,
      },
    };
  }

  return {
    props: {
      exact: false,
      posts,
    },
  };
};

const Post: FC<InferGetServerSidePropsType<typeof getServerSideProps>> = ({
  exact,
  post,
  posts,
}) => {
  if (exact) {
    return <p>TODO</p>; //<BlogPostPage post={post} />;
  }
  return <p>TODO multiple Posts</p>; // TODO
};

export default Post;
