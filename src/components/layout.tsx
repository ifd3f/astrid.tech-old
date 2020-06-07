import React, { ReactNode, FC } from "react"
import { Link } from "gatsby"
import { WindowLocation, NavigateFn } from "@reach/router"

import { rhythm, scale } from "../utils/typography"
import MainNavbar from "./navbar"
//import "../scss/main.scss"
import "bootstrap/dist/css/bootstrap.min.css"

interface LayoutProps {
  title: string
  children: ReactNode
}

const Layout: FC<LayoutProps> = ({ title, children }) => {
  return (
    <div
      style={{
        paddingTop: 70,
      }}
    >
      <MainNavbar />
      <main>{children}</main>
      <footer>
        © {new Date().getFullYear()}, Built with
        {` `}
        <a href="https://www.gatsbyjs.org">Gatsby</a>
      </footer>
    </div>
  )
}

export default Layout
