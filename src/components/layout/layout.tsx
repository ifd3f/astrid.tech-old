import { PageProps } from "gatsby"
import React, { FC, ReactNode, PropsWithChildren } from "react"
import FooterSection from "./footer"
import "./layout.scss"
import MainNavbar from "./navbar"

type LayoutProps = PageProps<any> & {
  children?: ReactNode
  mainClass: string
}

const Layout: FC<LayoutProps> = ({ children, mainClass }) => {
  return (
    <div className="root-wrapper">
      <MainNavbar />
      <main className={mainClass}>{children}</main>
      <FooterSection />
    </div>
  )
}

export default Layout
