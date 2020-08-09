import React, { FC } from "react"
import { useCookies } from "react-cookie"
import { Button, Container } from "reactstrap"
import { useStaticQuery } from "gatsby"
import { graphql } from "gatsby"
import { Site } from "src/types"

const COOKIE_NAME = "cookie-policy-notification"

type CookieNotificationDisplayProps = {
  onAcceptTerms: () => void
}

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
        <a onClick={onAcceptTerms} href="/privacy">
          Privacy Policy
        </a>{" "}
        for more information about what data is collected.
      </p>
      <Button onClick={onAcceptTerms} color="success" size="small">
        Accept!
      </Button>
    </Container>
  </div>
)

export const CookieNotification = () => {
  const [cookies, setCookie] = useCookies([COOKIE_NAME])
  const data: { site: Site } = useStaticQuery(graphql`
    {
      site {
        siteMetadata {
          cookiePolicyVersion
        }
      }
    }
  `)
  const isSSR = typeof window === "undefined"
  const { cookiePolicyVersion } = data.site.siteMetadata

  const onAcceptTerms = () => {
    setCookie(COOKIE_NAME, cookiePolicyVersion, {
      path: "/",
      maxAge: 365 * 24 * 3600,
    })
  }

  return isSSR || cookies[COOKIE_NAME] == cookiePolicyVersion ? null : (
    <CookieNotificationObject onAcceptTerms={onAcceptTerms} />
  )
}
