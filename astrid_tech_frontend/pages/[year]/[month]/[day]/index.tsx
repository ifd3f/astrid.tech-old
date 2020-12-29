import { useRouter } from "next/router";
import { FC } from "react";

const Page: FC = (props) => {
  const router = useRouter();
  return <p>test</p>;
};

export default Page;
