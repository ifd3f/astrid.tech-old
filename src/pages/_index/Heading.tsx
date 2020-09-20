import { graphql, Link, useStaticQuery } from "gatsby"
import React, { FC, ReactNode } from "react"
import { BsPerson } from "react-icons/bs"
import homepageStyles from "./heading.module.scss"
import styles from "./style.module.scss"
import { HomepageSectionProps } from "./util"

const APPX_BEGAN_PROGRAMMING = new Date("2013-02-01")
const APPX_BEGAN_HARDWARE = new Date("2015-12-15")

const HomepageHeader: FC<HomepageSectionProps> = ({ children, color }) => {
  return (
    <header
      className={homepageStyles.homepageHeader}
      style={{
        backgroundColor: color,
      }}
    >
      <div className={styles.sectionContent}>{children}</div>
    </header>
  )
}

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

const ImageOfMyself = () => (
  <div className={homepageStyles.imageSelf}>
    <BsPerson style={{ fontSize: 300 }} />
    TODO
  </div>
)

export function HeadingSection() {
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
    <HomepageHeader color="#F7A8B8">
      <div className={homepageStyles.nameWrapper}>
        <div className={homepageStyles.introductionGroup}>
          <p className={homepageStyles.preTitle}>Hello, my name is</p>
          <h1 className={homepageStyles.name}>Astrid Yu</h1>
          <p className={homepageStyles.postTitle}>Software Developer</p>
        </div>
        <ImageOfMyself />
      </div>
      <p className={homepageStyles.skillBrag}>
        An interactive portfolio made using{" "}
        <Link to="/projects/astrid-tech">
          React, Gatsby, and several other technologies
        </Link>
      </p>
    </HomepageHeader>
  )
}
