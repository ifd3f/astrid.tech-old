import { AppProps } from "next/app";
import React from "react";
import { TagTableProvider } from "../components/tags/TagTableProvider";
//require(`katex/dist/katex.min.css`);
import tags from "../data/tags";

export default function MyApp({ Component, pageProps }: AppProps) {
  return (
    <TagTableProvider tags={tags}>
      <Component {...pageProps} />
    </TagTableProvider>
  );
}
