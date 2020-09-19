import React from "react"
import Layout from "../components/layout/layout"
import SEO from "../components/seo"

const Homepage = () => {
  return (
    <Layout>
      <SEO
        title="Astrid Yu"
        description="My name is Astrid Yu and I create software. This is my website, and it's currently under construction!"
      />
      <div className={styles.homepageContainer}>
        <HeadingSection />
        <ElevatorSection />
        <SkillsSection />
        <EducationSection />
        <ExperienceSection />
        <ProjectsSection />
      </div>
    </Layout>
  )
}

export default Homepage
