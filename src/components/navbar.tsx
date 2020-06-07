import { Link } from "gatsby"
import React, { FC, useState } from "react"
import { BsArrowsCollapse } from "react-icons/bs"
import { GiHamburger } from "react-icons/gi"
import {
  Collapse,
  Nav,
  Navbar,
  NavbarBrand,
  NavbarToggler,
  NavItem,
  NavLink,
} from "reactstrap"

const MainNavbar: FC = () => {
  const [isOpen, setIsOpen] = useState(false)
  const toggleIsOpen = () => setIsOpen(!isOpen)

  return (
    <Navbar fixed="top" expand="md">
      <NavbarBrand href="/" activeClassName="active">
        Astrid
      </NavbarBrand>
      <NavbarToggler onClick={toggleIsOpen}>
        {isOpen ? <BsArrowsCollapse /> : <GiHamburger />}
      </NavbarToggler>
      <Collapse isOpen={isOpen} navbar>
        <Nav className="mr-auto" navbar>
          <NavLink href="/blog">fd</NavLink>
        </Nav>
      </Collapse>
    </Navbar>
  )
}

export default MainNavbar
