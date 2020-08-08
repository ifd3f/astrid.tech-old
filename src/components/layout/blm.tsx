import React from "react"

export const BLMBanner = () => (
  <div
    className="dark"
    style={{
      backgroundColor: "black",
      margin: 0,
      padding: 15,
      paddingLeft: 30,
      paddingRight: 30,
    }}
  >
    <p
      style={{
        color: "white",
        textAlign: "center",
        fontSize: "16pt",
        margin: 0,
      }}
    >
      <b>Black Lives Matter.</b> Please consider donating to the{" "}
      <a href="https://www.paypal.me/SLOBailFund">
        San Luis Obispo, CA Bail Fund
      </a>{" "}
      or <a href="https://bailfunds.github.io">other bail funds</a> so we can
      make the world a just and equitable place for all people of all colors.
    </p>
  </div>
)
