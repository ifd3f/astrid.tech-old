import { graphql, useStaticQuery } from "gatsby"
import React, { FC, ReactNode } from "react"
import { BsCodeSlash } from "react-icons/bs"
import { GiCircuitry } from "react-icons/gi"
import style from "./heading.module.scss"
import { HardwareYears, ProgrammingYears } from "./YearsSince"

const Headline = () => {
  return (
    <div className={style.headlineOuter}>
      <div className={style.headlineInner}>
        <div>
          <p className="lead">My name is</p>
          <h1 className={style.headlinePrimary}>ASTRID</h1>
          <p className="lead">and I'm a</p>
          <h1 className={style.headlinePrimary}>HACKER</h1>
        </div>
      </div>
    </div>
  )
}

interface IconInfoDisplayProps {
  imageSrc: string
  icon: ReactNode
  children: ReactNode
}

const IconInfoDisplay: FC<IconInfoDisplayProps> = ({
  icon,
  imageSrc,
  children,
}) => {
  return (
    <div
      className={style.subDisplayOuter}
      style={{
        background: `linear-gradient(rgba(0, 0, 0, 0.5), rgba(0, 0, 0, 0.5)), url(${imageSrc})`,
        backgroundSize: "cover",
        backgroundPosition: "center",
      }}
    >
      <div className={style.subDisplay}>
        <div className={style.wareIconOuter}>{icon}</div>
        <div className={style.wareTextOuter}>{children}</div>
      </div>
    </div>
  )
}

const HeadingSection = () => {
  const data = useStaticQuery(graphql`
    query HeadingBgQuery {
      coding: file(absolutePath: { regex: "/computer-coding.jpg/" }) {
        publicURL
      }
      electronics: file(absolutePath: { regex: "/electronics.jpg/" }) {
        publicURL
      }
    }
  `)

  const coding: string = data.coding.publicURL
  const electronics: string = data.electronics.publicURL

  return (
    <header className={style.homepageTop}>
      <Headline />
      <div className={style.subRow}>
        <IconInfoDisplay
          icon={<BsCodeSlash className={style.wareIcon} />}
          imageSrc={coding}
        >
          <p className={style.wareSecondary}>I've worked with</p>
          <h2 className={style.warePrimary}>SOFTWARE</h2>
          <p className={style.wareSecondary}>
            for{" "}
            <strong>
              <ProgrammingYears /> years
            </strong>
          </p>
        </IconInfoDisplay>
        <IconInfoDisplay
          icon={
            <GiCircuitry
              className={`${style.wareIcon} ${style.hardwareIcon}`}
            />
          }
          imageSrc={electronics}
        >
          <p className={style.wareSecondary}>as well as</p>
          <h2 className={style.warePrimary}>HARDWARE</h2>
          <p className={style.wareSecondary}>
            for{" "}
            <strong>
              <HardwareYears /> years
            </strong>
          </p>
        </IconInfoDisplay>
      </div>
    </header>
  )
}

/*
 <Col className="text-center">
          <p className="lead">My name is</p>
          <h1 className="display-1">ASTRID</h1>
          <p className="lead">and I'm a</p>
          <h1 className="display-1">HACKER</h1>
        </Col>
        <Row>
          <Col className="text-center" md={6}>
            <div className="d-inline-flex">
              <Col md="auto">
                <BsCodeSlash style={{fontSize: 100, height: "100%"}}/>
              </Col>
              <Col md="auto" className="text-left">
                <p>I've worked with</p>
                <h2>SOFTWARE</h2>
                <p>for <strong><ProgrammingYears/> years</strong></p>
              </Col>
            </div>
          </Col>
          <Col className="text-center" md={6}>
            <div className="d-inline-flex">
              <Col md="auto">
                <GiCircuitry style={{fontSize: 100, borderColor: "#FFFFFF", borderStyle: "solid"}}/>
              </Col>
              <Col md="auto" className="text-left">
                <p>as well as</p>
                <h2>HARDWARE</h2>
                <p>for <strong><HardwareYears/> years.</strong></p>
              </Col>
            </div>
          </Col>*/

export default HeadingSection
