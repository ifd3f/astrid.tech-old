import React, { ReactNode } from "react";
import { BsEnvelope } from "react-icons/bs";
import { FaBirthdayCake, FaLinkedin } from "react-icons/fa";
import { GiPhone } from "react-icons/gi";
import { FiDisc } from "react-icons/fi";
import { SiArchlinux } from "react-icons/si";
import { GoMarkGithub } from "react-icons/go";

export type HCardProperty = {
  title: ReactNode;
  children: ReactNode;
  classes?: string;
};

export const birthday: HCardProperty = {
  title: (
    <>
      <FaBirthdayCake title="Birthday" />
      Birthday
    </>
  ),
  children: (
    <a href="https://en.wikipedia.org/wiki/December_8">
      <time className="dt-bday" dateTime="2000-12-08">
        December 8
      </time>
    </a>
  ),
  classes: "",
};

export const timezone: HCardProperty = {
  title: "Timezone",
  children: (
    <data className="p-tz" value="-0700">
      PDT (UTC-7:00)
    </data>
  ),
};

export const pronouns: HCardProperty = {
  title: (
    <>
      <a
        href="https://en.wikipedia.org/wiki/Pronoun"
        style={{ fontVariant: "small-caps", fontSize: 10 }}
      >
        PRO
      </a>{" "}
      Pronouns
    </>
  ),
  children: (
    <>
      <span title="She went to the park" className="u-pronoun">
        she/her
      </span>{" "}
      or{" "}
      <span title="They made sushi yesterday" className="u-pronoun">
        they/them
      </span>
    </>
  ),
};

export const email: HCardProperty = {
  title: (
    <>
      <BsEnvelope title="Email" /> Email
    </>
  ),
  children: (
    <a className="u-email" href="mailto:astrid@astrid.tech">
      astrid@astrid.tech
    </a>
  ),
};
export const phone: HCardProperty = {
  title: (
    <>
      <GiPhone title="Phone" /> Phone
    </>
  ),
  children: (
    <a className="u-phone" href="tel:+18052705368">
      +1 ‪(805) 270-5368‬
    </a>
  ),
};

export const github: HCardProperty = {
  title: (
    <>
      <GoMarkGithub title="GitHub" /> GitHub
    </>
  ),
  children: (
    <a className="u-url" rel="me" href="https://github.com/astralbijection">
      astralbijection
    </a>
  ),
};

export const linkedin: HCardProperty = {
  title: (
    <>
      <FaLinkedin title="LinkedIn" /> LinkedIn
    </>
  ),
  children: (
    <a className="u-url" rel="me" href="https://linkedin.com/in/astrid-yu">
      astrid-yu
    </a>
  ),
};

export const favOS: HCardProperty = {
  title: (
    <>
      <FiDisc title="OS" /> Favorite OS
    </>
  ),
  children: (
    <a className="u-url u-os" href="https://archlinux.org">
      <SiArchlinux title="A" />
      rch btw
    </a>
  ),
};
