import { GetStaticProps, InferGetStaticPropsType } from "next";
import { FC } from "react";

const getStaticProps: GetStaticProps = () => {
  return {
    props: {},
  };
};

const Page: FC<InferGetStaticPropsType<typeof getStaticProps>> = () => {
  return null;
};

export default Page;
