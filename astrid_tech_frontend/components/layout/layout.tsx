import React, { FC, ReactNode } from "react";
import { CookiesProvider } from "react-cookie";
import { APIProvider } from "../api/APIProvider";
import { BLMBanner } from "./blm";
import { CookieNotification } from "./cookie-notification";
import FooterSection from "./footer";
import MainNavbar, { NavBarLinks } from "./navbar";

type LayoutProps = {
  children: ReactNode;
  showFooter?: boolean;
  doNotExpandHeight?: boolean;
  currentLocation?: NavBarLinks;
};

const Layout: FC<LayoutProps> = ({
  showFooter = true,
  children,
  doNotExpandHeight = false,
  currentLocation,
}) => {
  return (
    <CookiesProvider>
      <APIProvider root={process.env.ASTRID_TECH_API_ROOT}>
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
  );
};

export default Layout;
