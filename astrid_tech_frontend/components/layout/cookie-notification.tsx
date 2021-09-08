import { FC, useEffect, useState } from "react";
import { useCookies } from "react-cookie";
import Link from "next/link";
import { Button, Container } from "reactstrap";

const COOKIE_NAME = "cookie-policy-notification";

type CookieNotificationDisplayProps = {
  onAcceptTerms: () => void;
};

const CookieNotificationObject: FC<CookieNotificationDisplayProps> = ({
  onAcceptTerms,
}) => (
  <div
    style={{
      width: "100vw",
      position: "fixed",
      bottom: 0,
      backgroundColor: "white",
      paddingTop: 20,
      paddingBottom: 20,
    }}
  >
    <Container>
      <p>
        I use cookies on this site for analytics, as well as to make it more fun
        to use! Please see the{" "}
        <span onClick={onAcceptTerms}>
          <Link href="/privacy">Privacy Policy</Link>
        </span>{" "}
        for more information about what data is collected.
      </p>
      <Button onClick={onAcceptTerms} color="success" size="small">
        Accept!
      </Button>
    </Container>
  </div>
);

export const CookieNotification = () => {
  const [cookies, setCookie] = useCookies([COOKIE_NAME]);
  const [isSSR, setIsSSR] = useState(true);
  const cookiePolicyVersion = 1; // TODO change this

  useEffect(() => {
    setIsSSR(false);
  }, [isSSR]);

  const onAcceptTerms = () => {
    setCookie(COOKIE_NAME, cookiePolicyVersion, {
      path: "/",
      maxAge: 365 * 24 * 3600,
    });
  };

  return isSSR || cookies[COOKIE_NAME] == cookiePolicyVersion ? null : (
    <CookieNotificationObject onAcceptTerms={onAcceptTerms} />
  );
};
