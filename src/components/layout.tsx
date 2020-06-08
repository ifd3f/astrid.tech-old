import React, { ReactNode, FC } from "react"
import { Link } from "gatsby"
import { WindowLocation, NavigateFn } from "@reach/router"

import { rhythm, scale } from "../utils/typography"
import MainNavbar from "./navbar"
import "../scss/main.scss"
import FooterSection from "./footer"

interface LayoutProps {
  title: string
  children: ReactNode
}

const Layout: FC<LayoutProps> = ({ title, children }) => {
  return (
    <div className="root-wrapper">
      <MainNavbar />
      <main>{children}</main>
      <FooterSection />
    </div>
  )
}

export default Layout
