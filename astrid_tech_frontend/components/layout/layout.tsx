import { FC, ReactNode } from "react";
import { CookieNotification } from "./cookie-notification";
import FooterSection from "./footer";
import MainNavbar, { NavBarLinks } from "./navbar";
import classNames from "classnames";

type LayoutProps = {
  children: ReactNode;
  className?: string;
  showFooter?: boolean;
  doNotExpandHeight?: boolean;
  currentLocation?: NavBarLinks;
};

const Layout: FC<LayoutProps> = ({
  showFooter = true,
  className,
  children,
  doNotExpandHeight = false,
  currentLocation,
}) => {
  return (
    <>
      <MainNavbar currentLocation={currentLocation} fixed />
      <div
        className={classNames(
          "root-wrapper",
          doNotExpandHeight ? null : " expand-height",
          className
        )}
      >
        {children}
      </div>
      {showFooter ? <FooterSection /> : null}
      <CookieNotification />
    </>
  );
};

export default Layout;
