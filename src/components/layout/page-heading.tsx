import React, { FC } from "react"
import styles from "./page-heading.module.scss"

type PageHeadingProps = {
  title: string
  description: string
  bgColor: string
}

export const PageHeading: FC<PageHeadingProps> = ({
  title,
  description,
  bgColor,
}) => {
  return (
    <header className={styles.header} style={{ backgroundColor: bgColor }}>
      <h1>{title}</h1>
      <p>{description}</p>
    </header>
  )
}
