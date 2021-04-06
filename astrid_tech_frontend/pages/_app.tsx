import "@fontsource/ibm-plex-sans";
import "@fontsource/roboto-mono";
import "prismjs/themes/prism-tomorrow.css";
import "katex/dist/katex.min.css";
import { AppProps } from "next/app";
import React, { useEffect } from "react";
import { CookiesProvider } from "react-cookie";
import { APIProvider } from "../components/api/APIProvider";
import { TagTableProvider } from "../components/tags/TagTableProvider";
import tags from "../data/tags";
import "../styles/custom.scss";
import ReactGA from "react-ga";
import { useRouter } from "next/router";

ReactGA.initialize("UA-171109022-1");

export default function MyApp({ Component, pageProps }: AppProps) {
  const { pathname, query } = useRouter();

  useEffect(() => {
    ReactGA.pageview(pathname + query);
  }, [pathname, query]);

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
