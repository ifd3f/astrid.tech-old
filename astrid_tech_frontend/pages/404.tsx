import { InferGetStaticPropsType } from "next";
import { FC } from "react";
import { getStaticPaths } from "./[year]/[month]/[day]/[...slug]";

async function getAllPaths() {
  return getStaticPaths.getStringPaths!();
}

export const getStaticProps = async () => {
  const paths = await getAllPaths();
  console.log(paths);
  return { props: {} };
};

const Page: FC<InferGetStaticPropsType<typeof getStaticProps>> = ({}) => {
  return (
    <div>
      <p>test</p>
    </div>
  );
};

export default Page;
