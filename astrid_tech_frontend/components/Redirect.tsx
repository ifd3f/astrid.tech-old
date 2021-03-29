import { GetStaticProps, InferGetStaticPropsType } from "next";
import Head from "next/head";
import React, { FC, useEffect } from "react";
import { useRouter } from "next/router";
export const Redirect: FC<{ destination: string }> = ({ destination }) => {
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
