import "@fontsource/ibm-plex-sans";
import "@fontsource/red-hat-display";
import "firacode";
import "katex/dist/katex.min.css";
import { AppProps } from "next/app";
import React from "react";
import { TagTableProvider } from "../components/tags/TagTableProvider";
import tags from "../data/tags";
import "../styles/custom.scss";

export default function MyApp({ Component, pageProps }: AppProps) {
  return (
    <TagTableProvider tags={tags}>
      <Component {...pageProps} />
    </TagTableProvider>
  );
}
