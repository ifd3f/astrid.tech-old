import Link from "next/link";
import { FC, ReactNode } from "react";
import homepageStyles from "./heading.module.scss";
import Image from "next/image";

const APPX_BEGAN_PROGRAMMING = new Date("2013-02-01");
const APPX_BEGAN_HARDWARE = new Date("2015-12-15");

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

const ImageOfMyself = () => (
  <div className={homepageStyles.imageSelf + " u-photo"}>
    <img src="/avatar.jpg" alt="Astrid Yu" />
  </div>
);

export function HeadingSection() {
  return (
    <header className="homepage-header homepage-section">
      <div className={homepageStyles.nameWrapper + " h-card"}>
        <div className={homepageStyles.introductionGroup}>
          <p className={homepageStyles.preTitle}>
            <strong>Hello</strong>, my name is
          </p>
          <h1 className={homepageStyles.name + " p-name"}>Astrid Yu</h1>
          <p className={homepageStyles.postTitle}>
            and I develop <strong>software</strong>
          </p>
        </div>
      </div>
      <p className={homepageStyles.skillBrag}>
        Made using{" "}
        <Link href="/projects/astrid-tech">
          <a href="/projects/astrid-tech">
            Next.js, Django, and several other technologies
          </a>
        </Link>
      </p>
    </header>
  );
}
