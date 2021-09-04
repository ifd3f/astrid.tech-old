import { InferGetStaticPropsType } from "next";
import React, { FC } from "react";
import { FaRssSquare } from "react-icons/fa";
import { Container } from "reactstrap";
import { PostBrief } from "../components/blog/feed";
import { PageHeading } from "../components/layout";
import Layout from "../components/layout/layout";
import SEO from "../components/seo";
import { getBlogPosts } from "../lib/cache";
import { excerptify } from "../lib/markdown";
import styles from "../styles/blog.module.scss";
import { convertBlogPostToObjectDate } from "../types/types";

export const getStaticProps = async () => {
  const posts = await Promise.all(getBlogPosts().map(excerptify(120)));
  return {
    props: { posts },
  };
};

const Page: FC<InferGetStaticPropsType<typeof getStaticProps>> = ({
  posts,
}) => {
  const title = "Homepage";
  const meta = "Welcome to my website!";

  return (
    <Layout currentLocation="blog">
      <SEO title={title} description={meta} />
      <PageHeading title="Astrid Yu" bgColor="#1a237e" textColor="#ffffff" />
      <Container className={styles.blogContentContainer}>
        <section>
          <p className="text-right">
            <a href="https://astrid.tech/rss.xml">
              <FaRssSquare title="astrid.tech RSS" />
            </a>
          </p>
        </section>
        <section>
          <h2>Latest Posts</h2>
          {posts.map((post) => (
            <PostBrief
              key={post.slug}
              post={convertBlogPostToObjectDate(post)}
            />
          ))}
          <p className="text-center text-muted">(End of posts)</p>
        </section>
      </Container>
    </Layout>
  );
};

export default Page;
