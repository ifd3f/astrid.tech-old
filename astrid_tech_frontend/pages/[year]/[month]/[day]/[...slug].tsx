import { GetStaticPaths, GetStaticProps, InferGetStaticPropsType } from "next";
import { FC } from "react";
import BlogPostPage from "../../../../components/blog/blog";
import { getBlogPost, getBlogPostSlugs, Path } from "../../../../lib/cache";
import { renderMarkdown } from "../../../../lib/markdown";
import { BlogPost } from "../../../../types/types";

function pathToKey(path: Path) {
  const joined = path.slug.join(" ");
  return `${path.year}${path.month}${path.day}${joined}`;
}

export const getStaticPaths: GetStaticPaths = async () => {
  return {
    paths: getBlogPostSlugs().map((params) => ({
      params,
    })),
    fallback: false,
  };
};

export const getStaticProps: GetStaticProps = async ({ params }) => {
  const { post, assetRoot } = getBlogPost(params!! as Path);

  const content = await renderMarkdown(post.content);

  `select slug, tag from blog_post
  left join blog_tag
  on blog_post.id = blog_tag.fk_blog
  where tag in (select tag from blog_tag 
  where blog_tag.fk_blog = (select id from blog_post where slug = 'astrid-tech-v1'))
  `; // TODO do something with this

  return {
    props: { post: { ...post, content: content } },
  };
};

const Post: FC<InferGetStaticPropsType<typeof getStaticProps>> = ({ post }) => {
  const p = post as BlogPost<string>;
  return <BlogPostPage post={p} />;
};

export default Post;
