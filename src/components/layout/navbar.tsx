import { graphql, Link, useStaticQuery } from "gatsby"
import React, { FC, ReactNode, useState } from "react"
import { BsArrowsCollapse } from "react-icons/bs"
import { GiHamburger } from "react-icons/gi"
import { Collapse, Navbar, NavbarBrand, NavbarToggler } from "reactstrap"
import { Site } from "src/types"
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

type QueryData = {
  site: Site
}

const MainNavbar: FC<MainNavbarProps> = ({ currentLocation, fixed }) => {
  const [isOpen, setIsOpen] = useState(false)
  const toggleIsOpen = () => setIsOpen(!isOpen)
  const data: QueryData = useStaticQuery(graphql`
    query NavbarQuery {
      site {
        siteMetadata {
          title
        }
      }
    }
  `)

  return (
    <Navbar
      className="main-navbar"
      fixed={fixed ? "top" : undefined}
      expand="md"
    >
      <NavbarBrand tag={Link} to="/" activeClassName="active">
        {data.site.siteMetadata.title}
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
