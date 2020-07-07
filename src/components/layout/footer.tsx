import React, { FC } from "react"
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
    <a rel="license" href="http://creativecommons.org/licenses/by-sa/4.0/">
      <img
        alt="Creative Commons License"
        style={{ borderWidth: 0 }}
        src="https://i.creativecommons.org/l/by-sa/4.0/88x31.png"
      />
    </a>
    <br />
    <a
      href="http://purl.org/dc/dcmitype/InteractiveResource"
      property="dct:title"
      rel="dct:type"
    >
      The page content of astrid.tech
    </a>{" "}
    by{" "}
    <a
      href="https://astrid.tech"
      property="cc:attributionName"
      rel="cc:attributionURL"
    >
      Astrid Yu
    </a>{" "}
    is licensed under a{" "}
    <a rel="license" href="http://creativecommons.org/licenses/by-sa/4.0/">
      Creative Commons Attribution-ShareAlike 4.0 International License
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
        The source code of the astrid.tech is licensed under the{" "}
        <a href="https://www.gnu.org/licenses/agpl-3.0.en.html">AGPL License</a>
        .{" "}
      </p>
    </>
  )
}

const FooterSection = () => {
  const result = useStaticQuery(graphql`
    query GetFooterData {
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

  const buildDate = result.currentBuildDate.currentDate
  const agplLogo = result.agplLogo.childImageSharp.fixed

  return (
    <footer className={style.footer}>
      <Container>
        <p>Website was last built at {buildDate}.</p>
        <p>
          Created by Astrid Augusta Yu with a generous helping of <Tea /> and{" "}
          <Witch />. See the{" "}
          <Link to="/project/astrid-tech">self-referential project page</Link>{" "}
          or see the code yourself on{" "}
          <a href="https://github.com/Plenglin/astrid.tech">GitHub</a>.
        </p>
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
