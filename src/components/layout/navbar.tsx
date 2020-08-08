import { Link } from "gatsby"
import React, { FC, ReactNode, useState } from "react"
import { BsArrowsCollapse } from "react-icons/bs"
import { GiHamburger, GiBirdTwitter } from "react-icons/gi"
import { GoMarkGithub } from "react-icons/go"
import {
  Collapse,
  Navbar,
  NavbarBrand,
  NavbarToggler,
  NavLink,
  Row,
  NavbarText,
  NavItem,
} from "reactstrap"
import "./navbar.scss"

export type NavBarLinks = "brand" | "projects" | "blog" | "about"

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

const NavbarSeparator = () => <div className="navbar-separator" />

type MainNavbarProps = {
  currentLocation?: NavBarLinks
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
        <NavbarSeparator />
        <GNavLink to="/about" active={currentLocation == "about"}>
          About
        </GNavLink>
      </Collapse>
    </Navbar>
  )
}

export default MainNavbar
