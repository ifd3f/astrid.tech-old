import ElevatorSection from "./elevator"
import ExperienceSection from "./ExperienceSection"
import HeadingSection from "./Heading"
import ProjectsSection from "./Projects"
import SkillsSection from "./Skills"
import EducationSection from "./education"
import styles from "./style.module.scss"
import React from "react"

export default function HomepageContainer() {
  return (
    <main className={styles.homepageContainer}>
      <HeadingSection />
      <ElevatorSection />
      <SkillsSection />
      <EducationSection />
      <ExperienceSection />
      <ProjectsSection />
    </main>
  )
}
