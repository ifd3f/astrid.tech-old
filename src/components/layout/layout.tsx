import { PageProps } from "gatsby"
import React, { FC, ReactNode, PropsWithChildren } from "react"
import FooterSection from "./footer"
import "./layout.scss"
import MainNavbar, { NavBarLinks } from "./navbar"
import { BLMBanner } from "./blm"
import { CookieNotification } from "./cookie-notification"
import { CookiesProvider } from "react-cookie"

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
    <CookiesProvider>
      <MainNavbar currentLocation={currentLocation} fixed />
      <div
        className={"root-wrapper" + (doNotExpandHeight ? "" : " expand-height")}
      >
        <BLMBanner />
        {children}
      </div>
      {showFooter ? <FooterSection /> : null}
      <CookieNotification />
    </CookiesProvider>
  )
}

export default Layout
