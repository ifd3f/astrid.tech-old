import { Container } from "reactstrap";
import Layout from "../components/layout";
import { PageHeading } from "../components/layout/page-heading";
import SEO from "../components/seo";

/**
 * Force this page to get exported
 */
export async function getStaticProps() {
  return { props: {} };
}

const Page = () => {
  return (
    <Layout>
      <SEO title="Privacy Policy" description="What I do with your cookies" />
      <PageHeading
        title="Privacy Policy"
        bgColor="#333333"
        textColor="#ffffff"
      />
      <Container>
        <h2>What I use cookies for</h2>
        <ul>
          <li>
            Tracking and analyzing traffic to my website with Google Analytics
          </li>
          <li>
            Functional features, like allowing you to save commenter identity
            details
          </li>
        </ul>

        <h2>When I might log your IP address</h2>
        <ul>
          <li>
            By commenting on this website or reporting a comment, you consent to
            having your IP address logged as a measure to prevent abuse.
          </li>
        </ul>
      </Container>
    </Layout>
  );
};

export default Page;
