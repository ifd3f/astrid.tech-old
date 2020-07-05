import { Link } from "gatsby"
import React, { FC, ReactNode, useState } from "react"
import { BsArrowsCollapse } from "react-icons/bs"
import { GiHamburger } from "react-icons/gi"
import { Collapse, Navbar, NavbarBrand, NavbarToggler } from "reactstrap"
import "./navbar.scss"

type NavLinkProps = {
  to: string
  children: ReactNode
}

const NavLink: FC<NavLinkProps> = ({ to, children }) => {
  return (
    <Link className={`nav-link`} to={to}>
      {children}
    </Link>
  )
}

const MainNavbar: FC = () => {
  const [isOpen, setIsOpen] = useState(false)
  const toggleIsOpen = () => setIsOpen(!isOpen)

  return (
    <Navbar className="main-navbar" fixed="top" expand="md">
      <NavbarBrand tag={Link} to="/" activeClassName="active">
        Astrid's Tech
      </NavbarBrand>
      <NavbarToggler onClick={toggleIsOpen}>
        {isOpen ? <BsArrowsCollapse /> : <GiHamburger />}
      </NavbarToggler>
      <Collapse isOpen={isOpen} navbar>
        <NavLink to="/portfolio">Portfolio</NavLink>
        <NavLink to="/blog">Blog</NavLink>
      </Collapse>
    </Navbar>
  )
}

export default MainNavbar
