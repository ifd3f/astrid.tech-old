import { PageProps } from "gatsby"
import React, { FC, ReactNode, PropsWithChildren } from "react"
import FooterSection from "./footer"
import "./layout.scss"
import MainNavbar, { NavBarLinks } from "./navbar"
import { BLMBanner } from "./blm"

type LayoutProps = PageProps<any> & {
  children?: ReactNode
  className: string
  showFooter?: boolean
  currentLocation: NavBarLinks
}

const Layout: FC<LayoutProps> = ({
  showFooter = true,
  children,
  className,
  currentLocation,
}) => {
  return (
    <div className="root-wrapper">
      <MainNavbar fixed currentLocation={currentLocation} />
      <BLMBanner />
      <main className={className}>{children}</main>
      {showFooter ? <FooterSection /> : null}
    </div>
  )
}

export default Layout
