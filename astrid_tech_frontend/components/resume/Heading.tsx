import React, { FC, ReactNode } from "react";
import homepageStyles from "./heading.module.scss";
import styles from "./style.module.scss";
import { HomepageSectionProps } from "./util";

const APPX_BEGAN_PROGRAMMING = new Date("2013-02-01");
const APPX_BEGAN_HARDWARE = new Date("2015-12-15");

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
  );
};

type YearsSinceProps = {
  date: Date;
};

const YearsSince: FC<YearsSinceProps> = ({ date }) => {
  const rawYears = (Date.now() - date.getTime()) / (1000 * 24 * 3600 * 365);
  if (rawYears % 1.0 < 0.5) {
    return (
      <>
        OVER <strong>{Math.floor(rawYears)}</strong>
      </>
    );
  } else {
    return (
      <>
        AROUND <strong>{Math.ceil(rawYears)}</strong>
      </>
    );
  }
};

const ProgrammingYears = () => {
  return <YearsSince date={APPX_BEGAN_PROGRAMMING} />;
};

const HardwareYears = () => {
  return <YearsSince date={APPX_BEGAN_HARDWARE} />;
};

interface IconInfoDisplayProps {
  imageSrc: string;
  icon: ReactNode;
  children: ReactNode;
}

const ImageOfMyself = ({ image }: { image: any }) => (
  <div className={homepageStyles.imageSelf + " u-photo"}>
    <Img fluid={image} />
  </div>
);

export function HeadingSection() {
  const data = useStaticQuery(graphql`
    query HeadingBgQuery {
      avatar: file(relativePath: { eq: "avatar.jpg" }) {
        childImageSharp {
          fluid(maxHeight: 500) {
            ...GatsbyImageSharpFluid_tracedSVG
          }
        }
      }
    }
  `);

  const myself: any = data.avatar.childImageSharp.fluid;

  return (
    <header className="homepage-header homepage-section">
      <div className={homepageStyles.nameWrapper + " h-card"}>
        <div className={homepageStyles.introductionGroup}>
          <p className={homepageStyles.preTitle}>Hello, my name is</p>
          <h1 className={homepageStyles.name + " p-name"}>Astrid Yu</h1>
          <p className={homepageStyles.postTitle}>Software Developer</p>
        </div>
        <ImageOfMyself image={myself} />
      </div>
      <p className={homepageStyles.skillBrag}>
        An interactive portfolio made using{" "}
        <Link to="/projects/astrid-tech">
          React, Gatsby, and several other technologies
        </Link>
      </p>
    </header>
  );
}
