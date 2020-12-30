import { AppProps } from "next/app";
//import "../styles/custom.scss";

export default function MyApp({ Component, pageProps }: AppProps) {
  return <Component {...pageProps} />;
}
