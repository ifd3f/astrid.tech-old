import { Link } from "gatsby"
import React, { FC, ReactNode, useState, PropsWithChildren } from "react"
import { BsArrowsCollapse } from "react-icons/bs"
import { GiHamburger } from "react-icons/gi"
import {
  Collapse,
  Navbar,
  NavbarBrand,
  NavbarToggler,
  NavLink,
} from "reactstrap"
import "./navbar.scss"

export type NavBarLinks = "brand" | "projects" | "blog"

type NavLinkProps = {
  to: string
  children: ReactNode
  active: boolean
}

const GNavLink: FC<NavLinkProps> = ({ to, children, active }) => {
  return (
    <Link className={`nav-link ` + (active ? "active" : "")} to={to}>
      {children}
    </Link>
  )
}

type MainNavbarProps = {
  currentLocation: NavBarLinks
  fixed?: boolean
}

const MainNavbar: FC<MainNavbarProps> = ({ currentLocation, fixed }) => {
  const [isOpen, setIsOpen] = useState(false)
  const toggleIsOpen = () => setIsOpen(!isOpen)

  return (
    <Navbar
      className="main-navbar"
      fixed={fixed ? "top" : undefined}
      expand="md"
    >
      <NavbarBrand tag={Link} to="/" activeClassName="active">
        Astrid's Tech
      </NavbarBrand>
      <NavbarToggler onClick={toggleIsOpen}>
        {isOpen ? <BsArrowsCollapse /> : <GiHamburger />}
      </NavbarToggler>
      <Collapse isOpen={isOpen} navbar>
        <GNavLink to="/projects" active={currentLocation == "projects"}>
          Projects
        </GNavLink>
        <GNavLink to="/blog" active={currentLocation == "blog"}>
          Blog
        </GNavLink>
        <NavLink href="https://github.com/plenglin">GitHub</NavLink>
      </Collapse>
    </Navbar>
  )
}

export default MainNavbar
