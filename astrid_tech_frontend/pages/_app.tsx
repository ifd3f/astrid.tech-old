import { AppProps } from "next/app";
//import "../styles/custom.scss";
//require(`katex/dist/katex.min.css`);

export default function MyApp({ Component, pageProps }: AppProps) {
  return <Component {...pageProps} />;
}
