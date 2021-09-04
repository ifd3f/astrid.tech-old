import React, { FC, ReactNode } from 'react';
import { BLMBanner } from './blm';
import { CookieNotification } from './cookie-notification';
import FooterSection from './footer';
import MainNavbar, { NavBarLinks } from './navbar';

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
    <>
      <MainNavbar currentLocation={currentLocation} fixed />
      <div
        className={'root-wrapper' + (doNotExpandHeight ? '' : ' expand-height')}
      >
        {children}
      </div>
      {showFooter ? <FooterSection /> : null}
      <CookieNotification />
    </>
  );
};

export default Layout;
