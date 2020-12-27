import { graphql, useStaticQuery } from "gatsby"
import React, { FC, ReactNode } from "react"
import { CookiesProvider } from "react-cookie"
import { Site } from "src/types"
import { APIProvider } from "../api/APIProvider"
import { BLMBanner } from "./blm"
import { CookieNotification } from "./cookie-notification"
import FooterSection from "./footer"
import "./layout.scss"
import MainNavbar, { NavBarLinks } from "./navbar"

type LayoutProps = {
  children: ReactNode
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
  const data: { site: Site } = useStaticQuery(graphql`
    {
      site {
        siteMetadata {
          siteUrl
        }
      }
    }
  `)

  return (
    <CookiesProvider>
      <APIProvider root={data.site.siteMetadata.apiRoot}>
        <MainNavbar currentLocation={currentLocation} fixed />
        <div
          className={
            "root-wrapper" + (doNotExpandHeight ? "" : " expand-height")
          }
        >
          <BLMBanner />
          {children}
        </div>
        {showFooter ? <FooterSection /> : null}
        <CookieNotification />
      </APIProvider>
    </CookiesProvider>
  )
}

export default Layout
