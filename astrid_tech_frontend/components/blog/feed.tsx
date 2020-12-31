import moment from "moment";
import Link from "next/link";
import React, { FC } from "react";
import { FaRssSquare } from "react-icons/fa";
import { Col, Container, Row } from "reactstrap";
import { BlogPostMeta } from "../../types/types";
import { PageHeading } from "../layout";
import Layout from "../layout/layout";
import { default as style, default as styles } from "../scss/blog.module.scss";
import SEO from "../seo";
import { TagList } from "../tags/tag";

type PostProps = {
  post: BlogPostMeta<Date>;
};

export const PostBrief: FC<PostProps> = ({ post }) => {
  const dateString = moment(post.date).format("DD MMMM YYYY");

  return (
    <Link href={post.slug!}>
      <article className={style.brief}>
        <Row className="col">
          <Col sm={8}>
            <h3>{post.title}</h3>
            <div className="text-muted">{post.description}</div>
          </Col>
          <Col sm={4}>
            <p className={`text-muted ${style.date}`}>{dateString}</p>
            <TagList tags={post.tags} link limit={5} />
          </Col>
        </Row>
        <Col>
          <p>{post.description}</p>
        </Col>
      </article>
    </Link>
  );
};

export type BlogFeedProps = {
  posts: BlogPostMeta<Date>[];
};

export const BlogFeed: FC<BlogFeedProps> = ({ posts }) => {
  return (
    <section>
      {posts.map((post) => (
        <PostBrief key={post.slug} post={post} />
      ))}
      <p className="text-center text-muted">(End of posts)</p>
    </section>
  );
};

const BlogIndex: FC<PageProps<Data>> = (props) => {
  const { data } = props;
  const title = "Blog";
  const description = "Astrid Yu's designated mind dump location";
  return (
    <Layout currentLocation="blog">
      <SEO title={title} description={description} />
      <PageHeading title={title} description={description} bgColor="#eecc8d" />
      <Container className={styles.blogContentContainer}>
        <section>
          <p className="text-right">
            <a href="https://astrid.tech/rss.xml">
              <FaRssSquare title="Subscribe to the Blog!" />
            </a>
          </p>
        </section>
        <section>
          {data.allBlogPost.edges.map(({ node: post }) => (
            <PostBrief key={post.slug} post={post} />
          ))}
          <p className="text-center text-muted">(End of posts)</p>
        </section>
      </Container>
    </Layout>
  );
};

export default BlogIndex;
