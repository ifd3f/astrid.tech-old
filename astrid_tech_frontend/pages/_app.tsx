import "@fontsource/ibm-plex-sans";
import "@fontsource/roboto-mono";
import "prismjs/themes/prism-tomorrow.css";
import "katex/dist/katex.min.css";
import { AppProps } from "next/app";
import React from "react";
import { CookiesProvider } from "react-cookie";
import { APIProvider } from "../components/api/APIProvider";
import { TagTableProvider } from "../components/tags/TagTableProvider";
import tags from "../data/tags";
import "../styles/custom.scss";

export default function MyApp({ Component, pageProps }: AppProps) {
  return (
    <TagTableProvider tags={tags}>
      <CookiesProvider>
        <APIProvider root={process.env.apiRoot!}>
          <Component {...pageProps} />
        </APIProvider>
      </CookiesProvider>
    </TagTableProvider>
  );
}
