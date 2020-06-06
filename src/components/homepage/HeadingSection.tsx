import React, { FC, ReactNode } from "react";
import { imgCoding, imgHardware } from "../../../assets";
import { GiCircuitry, GiHamburger } from "react-icons/gi";
import { BsCodeSlash } from "react-icons/bs";

const APPX_BEGAN_PROGRAMMING = new Date("2013-02-01");
const APPX_BEGAN_HARDWARE = new Date("2015-12-15");

interface YearsSinceProps {
  date: Date;
}

const YearsSince: FC<YearsSinceProps> = ({ date }) => {
  const rawYears =
    (new Date().getTime() - date.getTime()) / (1000 * 24 * 3600 * 365);
  if (rawYears % 1.0 < 0.5) {
    return <>over {Math.floor(rawYears)}</>;
  } else {
    return <>around {Math.ceil(rawYears)}</>;
  }
};

const ProgrammingYears = () => {
  return <YearsSince date={APPX_BEGAN_PROGRAMMING} />;
};

const HardwareYears = () => {
  return <YearsSince date={APPX_BEGAN_HARDWARE} />;
};

const Headline = () => {
  return (
    <div className="headline-outer">
      <div className="headline-inner">
        <div>
          <p className="lead">My name is</p>
          <h1 className="headline-primary">ASTRID</h1>
          <p className="lead">and I'm a</p>
          <h1 className="headline-primary">HACKER</h1>
        </div>
      </div>
    </div>
  );
};

interface IconInfoDisplayProps {
  icon: ReactNode;
  imageSrc: string;
  children: ReactNode;
}

const IconInfoDisplay: FC<IconInfoDisplayProps> = ({
  icon,
  imageSrc,
  children,
}) => {
  return (
    <div
      className="subdisplay-outer"
      style={{
        background: `linear-gradient(rgba(0, 0, 0, 0.5), rgba(0, 0, 0, 0.5)), url(${imageSrc})`,
        backgroundSize: "cover",
        backgroundPosition: "center",
      }}
    >
      <div className="subdisplay-inner">
        <div className="ware-icon-outer">{icon}</div>
        <div className="ware-icon-outer">{children}</div>
      </div>
    </div>
  );
};

function HeadingSection() {
  return (
    <header className="homepage-top">
      <Headline />
      <div className="sub-row">
        <IconInfoDisplay
          icon={<BsCodeSlash className="ware-icon" />}
          imageSrc={imgCoding}
        >
          <p className="ware-secondary">I've worked with</p>
          <h2 className="ware-primary">SOFTWARE</h2>
          <p className="ware-secondary">
            for{" "}
            <strong>
              <ProgrammingYears /> years
            </strong>
          </p>
        </IconInfoDisplay>
        <IconInfoDisplay
          icon={<GiCircuitry className="ware-icon hardware-icon" />}
          imageSrc={imgHardware}
        >
          <p className="ware-secondary">as well as</p>
          <h2 className="ware-primary">HARDWARE</h2>
          <p className="ware-secondary">
            for{" "}
            <strong>
              <HardwareYears /> years
            </strong>
          </p>
        </IconInfoDisplay>
      </div>
    </header>
  );
}
