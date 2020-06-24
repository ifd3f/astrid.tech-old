import styles from "./style.module.scss"
import React, { ReactNode, FC } from "react"

type HomepageSectionProps = {
  children: ReactNode
  color?: string
}

export const HomepageSection: FC<HomepageSectionProps> = ({
  children,
  color,
}) => {
  return (
    <section
      style={{
        backgroundColor: color,
      }}
    >
      <div className={styles.sectionContent}>{children}</div>
    </section>
  )
}
