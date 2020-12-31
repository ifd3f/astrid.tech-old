import { GetStaticProps, InferGetStaticPropsType } from "next";
import { FC } from "react";
import { Container } from "reactstrap";
import Layout from "../components/layout";
import { getBlogPostMetas } from "../lib/cache";

export const getStaticProps: GetStaticProps = async () => {
  const posts = getBlogPostMetas();
  return {
    props: { posts },
  };
};

const Page: FC<InferGetStaticPropsType<typeof getStaticProps>> = ({
  posts,
}) => {
  return (
    <Layout>
      <Container>
        {posts.map((post) => (
          <p>{post.slug}</p>
        ))}
      </Container>
    </Layout>
  );
};

export default Page;
