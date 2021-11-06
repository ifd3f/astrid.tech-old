import { InferGetStaticPropsType } from "next";
import React, { FC } from "react";
import { FaRssSquare } from "react-icons/fa";
import { GrDocumentTxt } from "react-icons/gr";
import { BiCodeCurly } from "react-icons/bi";
import { Col, Container, Row } from "reactstrap";
import { PostBrief } from "../components/blog/feed";
import { PageHeading } from "../components/layout";
import Layout from "../components/layout/layout";
import SEO from "../components/seo";
import { getBlogPosts } from "../lib/cache";
import { excerptify } from "../lib/markdown";
import styles from "../styles/blog.module.scss";
import { convertBlogPostToObjectDate } from "../types/types";
import { useNSFW } from "components/nsfw";

export const getStaticProps = async () => {
  const posts = await Promise.all(getBlogPosts().map(excerptify(280)));
  return {
    props: { posts },
  };
};

const FeedLinks = () => (
  <>
    <p>My posts are also available in the following formats:</p>
    <ul>
      <li>
        <a href={`${process.env.publicRoot}/rss.xml`}>
          <FaRssSquare title="astrid.tech RSS Feed" /> RSS
        </a>
      </li>
      <li>
        <a href={`${process.env.publicRoot}/atom.xml`}>
          <FaRssSquare title="astrid.tech RSS Feed" /> Atom{" "}
        </a>
      </li>
      <li>
        <a href={`${process.env.publicRoot}/feed.json`}>
          <BiCodeCurly title="astrid.tech JSON Feed" /> JSON Feed{" "}
        </a>{" "}
        (<a href="https://www.jsonfeed.org/">more info</a>)
      </li>
      <li>
        <a href={`${process.env.publicRoot}/tw.txt`}>
          <GrDocumentTxt /> twtxt
        </a>{" "}
        (
        <a href="https://twtxt.readthedocs.io/en/stable/user/intro.html">
          more info
        </a>
        )
      </li>
    </ul>
  </>
);

export const BlogPage: FC<InferGetStaticPropsType<typeof getStaticProps>> = ({
  posts,
}) => {
  const meta = "Archives of all my posts";
  const { enabled: nsfwEnabled } = useNSFW();

  const blogFeed = (
    <>
      {posts
        .filter((post) => !post.tags.includes("nsfw") || nsfwEnabled)
        .map((post) => (
          <PostBrief key={post.slug} post={convertBlogPostToObjectDate(post)} />
        ))}
      <p className="text-center text-muted">(End of posts)</p>
    </>
  );

  return (
    <Layout currentLocation="blog">
      <SEO title="Blog" description={meta} />

      <PageHeading title="Blog Archive" bgColor="#1a237e" textColor="#ffffff" />

      <Container style={{ paddingTop: 20 }}>
        <Row className="justify-content-center">
          <Col className={styles.blogContentContainer} lg="8">
            {blogFeed}
          </Col>
          <Col className={styles.blogContentContainer} lg="4">
            <FeedLinks />
          </Col>
        </Row>
      </Container>
    </Layout>
  );
};

export default BlogPage;
