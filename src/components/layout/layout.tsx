import { PageProps } from "gatsby"
import React, { FC, ReactNode, PropsWithChildren } from "react"
import FooterSection from "./footer"
import "./layout.scss"
import MainNavbar from "./navbar"

type LayoutProps = PageProps<any> & {
  children?: ReactNode
  className: string
  showFooter?: boolean
}

const Layout: FC<LayoutProps> = ({
  showFooter = true,
  children,
  className,
}) => {
  return (
    <div className="root-wrapper">
      <MainNavbar />
      <main className={className}>{children}</main>
      {showFooter ? <FooterSection /> : null}
    </div>
  )
}

export default Layout
