import { Page } from "../components/resume";

/**
 * Force this page to get exported
 */
export async function getStaticProps() {
  return { props: {}};
}

const Index = ({}) => {
  return <Page />;
};

export default Index;
