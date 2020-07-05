import { PageProps } from "gatsby"
import React, { FC, ReactNode, PropsWithChildren } from "react"
import FooterSection from "./footer"
import "./layout.scss"
import MainNavbar from "./navbar"

type LayoutProps = PropsWithChildren<PageProps<undefined>>

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
