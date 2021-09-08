import { GetStaticProps, InferGetStaticPropsType } from "next";
import Head from "next/head";
import { FC, useEffect } from "react";
import { useRouter } from "next/router";

type RedirectProps = { destination: string };

export const Redirect: FC<RedirectProps> = ({ destination }) => {
  const router = useRouter();
  useEffect(() => {
    router.replace(destination);
  });
  return (
    <div>
      <Head>
        <meta httpEquiv="refresh" content={`0;url=${destination}`} />
      </Head>
      <p>
        This page has moved. If you haven't been redirected, please click{" "}
        <a href={destination}>here</a>.
      </p>
    </div>
  );
};

export default Redirect;
