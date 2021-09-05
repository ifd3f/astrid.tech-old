import Link from "next/link";
import { FC, ReactNode } from "react";
import { Col, Container, Row } from "reactstrap";
import style from "./footer.module.scss";
import Image from "next/image";
import packageJson from "../../package.json";
export const Tea = () => {
  return (
    <span title="tea" aria-label="tea" role="img">
      ☕
    </span>
  );
};

export const Witch = () => {
  return (
    <span title="witchcraft" aria-label="witchcraft" role="img">
      🧙‍
    </span>
  );
};

const ContentLicense = () => (
  <p>
    <a rel="license" href="http://creativecommons.org/licenses/by-nc-sa/4.0/">
      <img
        alt="Creative Commons License"
        style={{ borderWidth: 0 }}
        src="https://i.creativecommons.org/l/by-nc-sa/4.0/88x31.png"
      />
    </a>
    <br />
    <span
      {...{ "xmlns:dct": "http://purl.org/dc/terms/" }}
      property="dct:title"
    >
      The page content of astrid.tech
    </span>{" "}
    by{" "}
    <a
      {...{ "xmlns:cc": "http://creativecommons.org/ns#" }}
      href="https://astrid.tech"
      property="cc:attributionName"
      rel="cc:attributionURL"
    >
      Astrid Yu
    </a>{" "}
    is licensed under a{" "}
    <a rel="license" href="http://creativecommons.org/licenses/by-nc-sa/4.0/">
      Creative Commons Attribution-NonCommercial-ShareAlike 4.0 International
      License
    </a>
    .
  </p>
);

const AGPL: FC = () => {
  return (
    <>
      <a href="https://www.gnu.org/licenses/agpl-3.0.en.html">
        <img
          alt="GNU Affero General Public License"
          width={100}
          height={40}
          src="/agpl.png"
        />
      </a>
      <p>
        <a href="https://github.com/astralbijection/astrid.tech">
          The source code of astrid.tech
        </a>{" "}
        is licensed under the{" "}
        <a href="https://www.gnu.org/licenses/agpl-3.0.en.html">AGPL License</a>
        .{" "}
      </p>
    </>
  );
};

/**
 * Links for an IndieWebRing. For more info, see https://xn--sr8hvo.ws/dashboard
 * @returns links :)
 */
const WebRing: FC = () => {
  return (
    <>
      <a href="https://xn--sr8hvo.ws/%F0%9F%9A%AF%F0%9F%90%9E%F0%9F%8C%8A/previous">
        ←
      </a>
      An IndieWeb Webring 🕸💍
      <a href="https://xn--sr8hvo.ws/%F0%9F%9A%AF%F0%9F%90%9E%F0%9F%8C%8A/next">
        →
      </a>
    </>
  );
};

type SiteLinkProps = {
  href: string;
  children: ReactNode;
};

const SiteLink: FC<SiteLinkProps> = ({ href, children }) => (
  <Col xs={6} sm={4}>
    <Link href={href}>{children}</Link>
  </Col>
);

const FooterSection = () => {
  const version = packageJson.version;

  return (
    <footer className={style.footer}>
      <Container className="text-light small">
        <p style={{ textAlign: "center" }}>
          <WebRing />
        </p>
        <Col>
          <Row tag="nav">
            <SiteLink href="/privacy">Privacy Policy</SiteLink>
            <SiteLink href="/licenses">Open Source Licenses</SiteLink>
            <SiteLink href="/about">About/Contact</SiteLink>
          </Row>
        </Col>
        <Col>
          <p>
            astrid.tech v{version} was created by Astrid Yu with a generous
            helping of <Tea /> and <Witch />. See the{" "}
            <Link href="/projects/astrid-tech">
              self-referential project page
            </Link>{" "}
            or see the code yourself on{" "}
            <a href="https://github.com/astralbijection/astrid.tech">GitHub</a>.
          </p>
        </Col>
        <Row>
          <Col></Col>
        </Row>
        <Row>
          <Col className="text-center">
            <AGPL />
          </Col>
          <Col className="text-center">
            <ContentLicense />
          </Col>
        </Row>
        <a href="https://github.com/astralbijection/" rel="me"></a>
      </Container>
    </footer>
  );
};

export default FooterSection;
