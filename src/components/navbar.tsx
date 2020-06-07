import React, { FC, useState } from "react"
import { BsArrowsCollapse } from "react-icons/bs"
import { GiHamburger } from "react-icons/gi"
import { NavLink as RRNavLink } from "react-router-dom"
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
      <NavbarBrand to="/" activeClassName="active" tag={RRNavLink}>
        Astrid
      </NavbarBrand>
      <NavbarToggler onClick={toggleIsOpen}>
        {isOpen ? <BsArrowsCollapse /> : <GiHamburger />}
      </NavbarToggler>
      <Collapse isOpen={isOpen} navbar>
        <Nav className="mr-auto" navbar>
          <NavItem>
            <NavLink to="/blog" activeClassName="active" tag={RRNavLink}>
              Blog
            </NavLink>
          </NavItem>
          <NavItem>
            <NavLink to="/projects" activeClassName="active" tag={RRNavLink}>
              Projects
            </NavLink>
          </NavItem>
          <NavItem>
            <NavLink href="https://github.com/Plenglin">GitHub</NavLink>
          </NavItem>
        </Nav>
      </Collapse>
    </Navbar>
  )
}

export default MainNavbar
