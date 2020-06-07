import React from "react"
import { Container } from "reactstrap"
import { Link } from "gatsby"

export const Tea = () => {
  return (
    <span aria-label="tea" role="img">
      ☕
    </span>
  )
}

export const Witch = () => {
  return (
    <span aria-label="witchcraft" role="img">
      🧙‍
    </span>
  )
}

const FooterSection = () => {
  return (
    <footer>
      <Container>
        <p>
          Created by Astrid Augusta Yu with <Tea /> and <Witch /> . See the{" "}
          <Link to="/projects/astrid.tech">Colophon</Link> or try to figure out
          the spaghetti code yourself on{" "}
          <a href="https://github.com/Plenglin/astrid.tech">GitHub</a>
        </p>
      </Container>
    </footer>
  )
}

export default FooterSection
