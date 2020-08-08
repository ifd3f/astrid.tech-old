import { PageProps } from "gatsby"
import React, { FC, ReactNode, PropsWithChildren } from "react"
import FooterSection from "./footer"
import "./layout.scss"
import MainNavbar, { NavBarLinks } from "./navbar"
import { BLMBanner } from "./blm"

type LayoutProps = PageProps<any> & {
  children?: ReactNode
  showFooter?: boolean
  doNotExpandHeight?: boolean
  currentLocation?: NavBarLinks
}

const Layout: FC<LayoutProps> = ({
  showFooter = true,
  children,
  doNotExpandHeight = false,
  currentLocation,
}) => {
  return (
    <>
      <MainNavbar fixed currentLocation={currentLocation} />
      <div
        className={"root-wrapper" + (doNotExpandHeight ? "" : " expand-height")}
      >
        <BLMBanner />
        {children}
      </div>
      {showFooter ? <FooterSection /> : null}
    </>
  )
}

export default Layout
