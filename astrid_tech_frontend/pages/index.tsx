import { AstridTechAPI } from "lib/astrid-tech-api";
import { getPathFromEntry } from "lib/cache";
import { InferGetServerSidePropsType, InferGetStaticPropsType } from "next";
import React, { FC } from "react";
import { FaRssSquare } from "react-icons/fa";
import { Container } from "reactstrap";
import { PostBrief, PostBriefData } from "../components/blog/feed";
import { PageHeading } from "../components/layout";
import Layout from "../components/layout/layout";
import SEO from "../components/seo";
import styles from "../styles/blog.module.scss";

export const getServerSideProps = async () => {
  const api = AstridTechAPI.createWithEnvRoot();
  const posts = await api.getEntries();
  return {
    props: {
      posts: posts.map(
        (p) =>
          ({
            title: p.title,
            description: p.description,
            excerpt: "TODO excerpt", // TODO
            slug: "/" + getPathFromEntry(p).join("/"),
            tags: p.tags,
            publishedDate: p.published_date,
          } as PostBriefData)
      ).sort((a, b) => b.publishedDate.localeCompare(a.publishedDate)),
    },
  };
};

const Page: FC<InferGetServerSidePropsType<typeof getServerSideProps>> = ({
  posts,
}) => {
  const title = "Blog";
  const description = "Astrid Yu's designated mind dump location";
  return (
    <Layout currentLocation="brand">
      <SEO title={title} description={description} />
      <Container className={styles.blogContentContainer}>
        <header>
          <h1 className="p-name">Astrid Yu</h1>
        </header>
        <section>
          <p className="text-right">
            <a href="https://astrid.tech/rss.xml">
              <FaRssSquare title="astrid.tech RSS" />
            </a>
          </p>
        </section>
        <section>
          {posts.map((post) => (
            <PostBrief key={post.slug} post={post} />
          ))}
          <p className="text-center text-muted">(End of posts)</p>
        </section>
      </Container>
    </Layout>
  );
};

export default Page;
