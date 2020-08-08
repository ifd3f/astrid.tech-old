import React, { FC, ReactNode } from "react"
import { Container, Col, Row } from "reactstrap"
import { Link, useStaticQuery, graphql } from "gatsby"
import Img from "gatsby-image"
import style from "./footer.module.scss"

export const Tea = () => {
  return (
    <span title="tea" aria-label="tea" role="img">
      ☕
    </span>
  )
}

export const Witch = () => {
  return (
    <span title="witchcraft" aria-label="witchcraft" role="img">
      🧙‍
    </span>
  )
}

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
)

const AGPL: FC<{ logo: any }> = ({ logo }) => {
  return (
    <>
      <a href="https://www.gnu.org/licenses/agpl-3.0.en.html">
        <Img fixed={logo} alt="GNU Affero General Public License" />
      </a>
      <p>
        <a href="https://github.com/plenglin/astrid.tech">
          The source code of astrid.tech
        </a>{" "}
        is licensed under the{" "}
        <a href="https://www.gnu.org/licenses/agpl-3.0.en.html">AGPL License</a>
        .{" "}
      </p>
    </>
  )
}

type SiteLinkProps = {
  to: string
  children: ReactNode
}

const SiteLink: FC<SiteLinkProps> = ({ to, children }) => (
  <Col>
    <Link to={to}>{children}</Link>
  </Col>
)

const FooterSection = () => {
  const result = useStaticQuery(graphql`
    query GetFooterData {
      site {
        siteMetadata {
          version
        }
      }
      currentBuildDate {
        currentDate
      }
      agplLogo: file(relativePath: { eq: "agpl.png" }) {
        childImageSharp {
          fixed(height: 40) {
            ...GatsbyImageSharpFixed
          }
        }
      }
    }
  `)

  const version = result.site.siteMetadata.version
  const buildDate = result.currentBuildDate.currentDate
  const agplLogo = result.agplLogo.childImageSharp.fixed

  return (
    <footer className={style.footer}>
      <Container className="text-light">
        <Col>
          <Row tag="nav">
            <SiteLink to="/privacy">Privacy Policy</SiteLink>
          </Row>
        </Col>
        <Col>
          <p>Website was last updated on {buildDate}.</p>
          <p>
            astrid.tech v{version} was created by Astrid Yu with a generous
            helping of <Tea /> and <Witch />. See the{" "}
            <Link to="/projects/astrid-tech">
              self-referential project page
            </Link>{" "}
            or see the code yourself on{" "}
            <a href="https://github.com/Plenglin/astrid.tech">GitHub</a>.
          </p>
        </Col>
        <Row>
          <Col className="text-center">
            <AGPL logo={agplLogo} />
          </Col>
          <Col className="text-center">
            <ContentLicense />
          </Col>
        </Row>
      </Container>
    </footer>
  )
}

export default FooterSection
