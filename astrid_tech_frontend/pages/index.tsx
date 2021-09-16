import CardTable from "components/card/CardTable";
import * as vs from "components/card/values";
import { InferGetStaticPropsType } from "next";
import Link from "next/link";
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
  const posts = await Promise.all(
    getBlogPosts().slice(0, 7).map(excerptify(280))
  );
  return {
    props: { posts },
  };
};

const hCardValues = [
  vs.name,
  vs.pronouns,
  vs.hobbies,
  vs.occupation,
  vs.email,
  vs.linux,
  vs.website,
  vs.github,
  vs.matrix,
];

const Page: FC<InferGetStaticPropsType<typeof getStaticProps>> = ({
  posts,
}) => {
  const description =
    "My personal corner of the internet devoted to tech shenanigans and other stuff";

  const blogFeed = (
    <>
      <h2>
        Latest Posts{" "}
        <a href={`${process.env.publicRoot}/atom.xml`}>
          <FaRssSquare title="astrid.tech Atom Feed" />
        </a>
      </h2>

      {posts.map((post) => (
        <PostBrief key={post.slug} post={convertBlogPostToObjectDate(post)} />
      ))}
      <p className="text-center text-muted">
        <Link href="/blog">See more posts</Link>
      </p>
    </>
  );

  return (
    <Layout currentLocation="home">
      <SEO title="Astrid Yu" description={description} />

      <PageHeading title="astrid.tech" bgColor="#1a237e" textColor="#ffffff">
        <p className="p-note">{description}</p>
      </PageHeading>

      <Container style={{ paddingTop: 20 }}>
        <Row>
          <Col className={styles.blogContentContainer} lg="8">
            {blogFeed}
          </Col>
          <Col className="h-card" tag="article" xs="12" lg="4">
            <h2>Quick facts about me</h2>
            <CardTable fields={hCardValues} />{" "}
            <p style={{ textAlign: "right" }}>
              <Link href="/about">For more info, see /about.</Link>
            </p>
          </Col>
        </Row>
      </Container>
    </Layout>
  );
};

export default Page;
