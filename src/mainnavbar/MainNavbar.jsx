import React, { useState } from "react";
import { NavLink as RRNavLink } from "react-router-dom";
import {
  Collapse,
  DropdownItem,
  DropdownMenu,
  DropdownToggle,
  Nav,
  Navbar,
  NavbarBrand,
  NavbarToggler,
  NavItem,
  NavLink,
  UncontrolledDropdown,
} from "reactstrap";
import { GiHamburger } from "react-icons/gi";
import { BsArrowUp, BsArrowsCollapse } from "react-icons/bs";

function MainNavbar() {
  const [isOpen, setIsOpen] = useState(false);
  const toggleIsOpen = () => setIsOpen(!isOpen);

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
            <NavLink to="/works" activeClassName="active" tag={RRNavLink}>
              My Work
            </NavLink>
          </NavItem>
          <NavItem>
            <NavLink href="https://github.com/Plenglin">GitHub</NavLink>
          </NavItem>
        </Nav>
      </Collapse>
    </Navbar>
  );
}

export default MainNavbar;
