import { GetStaticProps, InferGetServerSidePropsType } from "next";
import { getBlogPosts } from "../../../../lib/content";
import { BlogPost } from "../../../../types/types";

export const getStaticPaths = async () => {
  const posts = await Promise.all(await getBlogPosts());
  const paths = posts.map((post) => ({
    params: {
      year: post.date.getFullYear().toString(),
      month: post.date.getMonth().toString(),
      day: post.date.getDay().toString(),
      slug: [post.slug],
      post: { ...post, date: post.date.toDateString() } as BlogPost<string>,
    },
  }));
  return {
    paths: paths,
    fallback: false,
  };
};

export const getStaticProps: GetStaticProps = async ({ params }) => {
  const post = (params!!.post as unknown) as BlogPost<string>;
  console.log(post);
  return {
    props: { post: post },
  };
};

const Post: InferGetServerSidePropsType<typeof getStaticProps> = ({ post }) => {
  return <p>{post}</p>;
};

export default Post;
