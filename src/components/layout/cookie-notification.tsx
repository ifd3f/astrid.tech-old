import React, { FC } from "react"
import { useCookies } from "react-cookie"
import { Button, Container } from "reactstrap"

const CLOSED_COOKIE = "closed-cookie-provider"

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
  const [cookies, setCookie] = useCookies([CLOSED_COOKIE])

  const onAcceptTerms = () => {
    setCookie(CLOSED_COOKIE, true, { path: "/", sameSite: true })
  }

  console.log(cookies[CLOSED_COOKIE])
  return cookies[CLOSED_COOKIE] ? null : (
    <CookieNotificationObject onAcceptTerms={onAcceptTerms} />
  )
}
