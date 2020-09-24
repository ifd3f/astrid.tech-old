import React from "react"
import Layout from "../components/layout/layout"
import SEO from "../components/seo"
import {
  EducationSection,
  ExperienceSection,
  HeadingSection,
  SkillsSection,
} from "./_index"
import styles from "./_index/style.module.scss"

const Homepage = () => {
  return (
    <Layout>
      <SEO
        title="Astrid Yu"
        description="My name is Astrid Yu and I create software and other cool things. Welcome to my website!"
      />
      <div className={styles.homepageContainer}>
        <HeadingSection />
        <SkillsSection />
        <EducationSection />
        <ExperienceSection />
      </div>
    </Layout>
  )
}

export default Homepage
