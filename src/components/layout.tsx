import React, { FC, ReactNode } from "react"
import "../scss/main.scss"
import FooterSection from "./footer"
import MainNavbar from "./navbar"

interface LayoutProps {
  children: ReactNode
}

const Layout: FC<LayoutProps> = ({ children }) => {
  return (
    <>
      <div className="root-wrapper">
        <MainNavbar />
        <main>{children}</main>
        <FooterSection />
      </div>
    </>
  )
}

export default Layout
