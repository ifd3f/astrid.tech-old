import classNames from "classnames";
import Link from "next/link";
import { FC, ReactNode, useState } from "react";
import { BsArrowsCollapse } from "react-icons/bs";
import { GiHamburger } from "react-icons/gi";
import { Collapse, Navbar, NavbarBrand, NavbarToggler } from "reactstrap";

export type NavBarLinks =
  | "home"
  | "projects"
  | "blog"
  | "now"
  | "about";

type NavLinkProps = {
  to: string;
  children: ReactNode;
  active: boolean;
};

const GNavLink: FC<NavLinkProps> = ({ to, children, active }) => {
  return (
    <li>
      <Link href={to} passHref>
        <a className={classNames("nav-link", active ? "active" : null)}>
          {children}
        </a>
      </Link>
    </li>
  );
};

const NavbarSeparator = () => <div className="navbar-separator" />;

type MainNavbarProps = {
  currentLocation?: NavBarLinks;
  fixed?: boolean;
};

const MainNavbar: FC<MainNavbarProps> = ({ currentLocation, fixed }) => {
  const [isOpen, setIsOpen] = useState(false);
  const toggleIsOpen = () => setIsOpen(!isOpen);

  return (
    <Navbar
      className="main-navbar"
      style={{ paddingLeft: 10, paddingRight: 10 }}
      fixed={fixed ? "top" : undefined}
      expand="md"
    >
      <Link href="/" passHref>
        <NavbarBrand
          tag="a"
          className={classNames(
            "nav-link",
            currentLocation == "home" ? "active" : null
          )}
          rel="home"
        >
          astrid.tech
        </NavbarBrand>
      </Link>
      <NavbarToggler onClick={toggleIsOpen}>
        {isOpen ? <BsArrowsCollapse /> : <GiHamburger />}
      </NavbarToggler>
      <Collapse
        isOpen={isOpen}
        navbar
        tag="ul"
        style={{
          marginBottom: 0,
          marginLeft: 0,
          paddingLeft: 0,
          listStyleType: "none",
        }}
      >
        <GNavLink to="/blog" active={currentLocation == "blog"}>
          Blog
        </GNavLink>
        <GNavLink to="/projects" active={currentLocation == "projects"}>
          Projects
        </GNavLink>

        <NavbarSeparator />

        <GNavLink to="/now" active={currentLocation == "now"}>
          Now
        </GNavLink>
        <GNavLink to="/about" active={currentLocation == "about"}>
          About
        </GNavLink>
      </Collapse>
    </Navbar>
  );
};

export default MainNavbar;
