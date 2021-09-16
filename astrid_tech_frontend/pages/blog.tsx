import * as vs from "components/card/values";
import { InferGetStaticPropsType } from "next";
import React, { FC } from "react";
import { FaRssSquare } from "react-icons/fa";
import { Col, Container, Row } from "reactstrap";
import { PostBrief } from "../components/blog/feed";
import { PageHeading } from "../components/layout";
import Layout from "../components/layout/layout";
import SEO from "../components/seo";
import { getBlogPosts } from "../lib/cache";
import { excerptify } from "../lib/markdown";
import styles from "../styles/blog.module.scss";
import { convertBlogPostToObjectDate } from "../types/types";

export const getStaticProps = async () => {
  const posts = await Promise.all(getBlogPosts().map(excerptify(280)));
  return {
    props: { posts },
  };
};

const Page: FC<InferGetStaticPropsType<typeof getStaticProps>> = ({
  posts,
}) => {
  const meta = "Archives of all my posts";

  const blogFeed = (
    <>
      <a href={`${process.env.publicRoot}/atom.xml`}>
        <FaRssSquare title="astrid.tech Atom Feed" />
      </a>
      {posts.map((post) => (
        <PostBrief key={post.slug} post={convertBlogPostToObjectDate(post)} />
      ))}
      <p className="text-center text-muted">(End of posts)</p>
    </>
  );

  return (
    <Layout currentLocation="blog">
      <SEO title="Blog" description={meta} />

      <PageHeading
        title="Blog Archive"
        bgColor="#1a237e"
        textColor="#ffffff"
      />

      <Container style={{ paddingTop: 20 }}>
        <Row className="justify-content-center">
          <Col className={styles.blogContentContainer} lg="8">
            {blogFeed}
          </Col>
        </Row>
      </Container>
    </Layout>
  );
};

export default Page;
