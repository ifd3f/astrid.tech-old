import AsyncRedis from "async-redis";
import { GetStaticPaths, GetStaticProps, InferGetStaticPropsType } from "next";
import { FC } from "react";
import { getBlogPosts } from "../../../../cacher/content";
import { BlogPost } from "../../../../types/types";

type Path = { year: string; month: string; day: string; slug: string[] };

function pathToKey(path: Path) {
  const joined = path.slug.join(" ");
  return `${path.year}${path.month}${path.day}${joined}`;
}

export const getStaticPaths: GetStaticPaths = async () => {
  const posts = await Promise.all(await getBlogPosts());

  const withPath = posts.map((post) => ({
    path: {
      year: post.date.getFullYear().toString(),
      month: (post.date.getMonth() + 1).toString(),
      day: (post.date.getDate() + 1).toString(),
      slug: [post.slug],
    } as Path,
    post,
  }));

  const client = AsyncRedis.createClient();
  await client.flushall();
  await Promise.all(
    withPath.map(({ path, post }) =>
      client.set(
        pathToKey(path),
        JSON.stringify({ ...post, date: post.date.toISOString() })
      )
    )
  );
  await client.quit();

  return {
    paths: withPath.map(({ path }) => ({
      params: path,
    })),
    fallback: false,
  };
};

export const getStaticProps: GetStaticProps = async ({ params }) => {
  const client = AsyncRedis.createClient();
  const postJson = ((await client.get(
    pathToKey((params!! as unknown) as Path)
  )) as unknown) as string;
  const post = JSON.parse(postJson) as BlogPost<string>;
  console.log(post);
  await client.quit();

  return {
    props: { post },
  };
};

const Post: FC<InferGetStaticPropsType<typeof getStaticProps>> = ({ post }) => {
  const p = post as BlogPost<string>;
  return <p>{p.content}</p>;
};

export default Post;
