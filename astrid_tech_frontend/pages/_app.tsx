import "@fontsource/ibm-plex-sans";
import "@fontsource/roboto-mono";
import "prismjs/themes/prism-tomorrow.css";
import "katex/dist/katex.min.css";
import { AppProps } from "next/app";
import { useEffect } from "react";
import { CookiesProvider } from "react-cookie";
import { APIProvider } from "../components/api/APIProvider";
import { NSFWProvider } from "../components/nsfw";
import { TagTableProvider } from "../components/tags/TagTableProvider";
import tags from "../data/tags";
import "../styles/custom.scss";
import ReactGA from "react-ga";
import { useRouter } from "next/router";

ReactGA.initialize("UA-171109022-1");

export default function MyApp({ Component, pageProps }: AppProps) {
  const { asPath, isReady } = useRouter();

  // Hook for Google Analytics
  useEffect(() => {
    if (!isReady) {
      return;
    }
    ReactGA.pageview(asPath);
  }, [asPath, isReady]);

  return (
    <TagTableProvider tags={tags}>
      <CookiesProvider>
        <APIProvider root={process.env.apiRoot!}>
          <NSFWProvider>
            <Component {...pageProps} />
          </NSFWProvider>
        </APIProvider>
      </CookiesProvider>
    </TagTableProvider>
  );
}
