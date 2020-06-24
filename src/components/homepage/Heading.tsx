import { graphql, useStaticQuery } from "gatsby"
import React, { FC, ReactNode } from "react"
import { BsCodeSlash } from "react-icons/bs"
import { GiCircuitry } from "react-icons/gi"
import styles from "./style.module.scss"
import { HomepageHeader } from "./util"

const APPX_BEGAN_PROGRAMMING = new Date("2013-02-01")
const APPX_BEGAN_HARDWARE = new Date("2015-12-15")

type YearsSinceProps = {
  date: Date
}

const YearsSince: FC<YearsSinceProps> = ({ date }) => {
  const rawYears = (Date.now() - date.getTime()) / (1000 * 24 * 3600 * 365)
  if (rawYears % 1.0 < 0.5) {
    return (
      <>
        OVER <strong>{Math.floor(rawYears)}</strong>
      </>
    )
  } else {
    return (
      <>
        AROUND <strong>{Math.ceil(rawYears)}</strong>
      </>
    )
  }
}

const ProgrammingYears = () => {
  return <YearsSince date={APPX_BEGAN_PROGRAMMING} />
}

const HardwareYears = () => {
  return <YearsSince date={APPX_BEGAN_HARDWARE} />
}

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
    <HomepageHeader>
      <div className={styles.nameWrapper}>
        <div className={styles.name}>
          <p className={styles.preTitle}>Hello, my name is</p>
          <h1>Astrid Yu</h1>
          <p className={styles.postTitle}>Software Developer</p>
        </div>
      </div>
    </HomepageHeader>
  )
}

export default HeadingSection
