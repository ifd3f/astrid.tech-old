import React from "react"
import { Container } from "reactstrap"
import { Link } from "gatsby"

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

const FooterSection = () => {
  return (
    <footer>
      <Container>
        <p>
          Created by Astrid Augusta Yu with a generous helping of <Tea /> and{" "}
          <Witch />. See the{" "}
          <Link to="/project/astrid-tech">self-referential project page</Link>{" "}
          or try to figure out the spaghetti code yourself on{" "}
          <a href="https://github.com/Plenglin/astrid.tech">GitHub</a>.
        </p>
        <p>
          The source code of the website is covered by the{" "}
          <a href="https://mit-license.org/">MIT License</a>.{" "}
        </p>
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
      </Container>
    </footer>
  )
}

export default FooterSection
