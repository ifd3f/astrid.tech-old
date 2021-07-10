import React from "react";
import Layout from "../layout";
import SEO from "../seo";
import { ExperienceSection } from "./ExperienceSection";
import { HeadingSection } from "./Heading";
import { SkillsSection } from "./Skills";
import styles from "./style.module.scss";

export const Page = ({}) => {
  return (
    <Layout currentLocation="resume">
      <SEO
        title="Resume"
        description="My resume, listing my skills and experience."
      />
      <div className={styles.homepageContainer}>
        <HeadingSection />
        {/*<TestmoninalSection />*/}
        <SkillsSection />
        <ExperienceSection />
        {/*<ProjectsSection />*/}
      </div>
    </Layout>
  );
};
