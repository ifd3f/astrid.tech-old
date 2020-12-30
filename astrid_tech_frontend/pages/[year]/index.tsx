import { useRouter } from "next/router";
import { FC } from "react";

const Page: FC<{ params: { year: string } }> = ({ params }) => {
  const router = useRouter();
  return null;
};
export default Page;
