import React, { FC, ReactNode } from "react"
import styles from "./page-heading.module.scss"

type PageHeadingProps = {
  title: string
  description: string
  bgColor: string
  above?: ReactNode
}

export const PageHeading: FC<PageHeadingProps> = ({
  title,
  description,
  bgColor,
  above,
}) => {
  return (
    <div style={{ backgroundColor: bgColor }}>
      <nav className={styles.above}>{above}</nav>
      <header className={styles.header}>
        <h1>{title}</h1>
        <p>{description}</p>
      </header>
    </div>
  )
}
