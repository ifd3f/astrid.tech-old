import React, { ReactNode } from "react";
import { BsEnvelope } from "react-icons/bs";
import {
  FaAddressCard,
  FaBirthdayCake,
  FaCubes,
  FaDice,
  FaGlobe,
  FaInstagram,
  FaLinkedin,
  FaReddit,
  FaTwitter,
} from "react-icons/fa";
import { FiDisc } from "react-icons/fi";
import { GoMarkGithub } from "react-icons/go";
import { SiHackaday, SiMatrix, SiNixos } from "react-icons/si";

export type HCardProperty = {
  key: string;
  title: ReactNode;
  children: ReactNode;
  classes?: string;
};

export const name: HCardProperty = {
  key: "name-en",
  title: "Name",
  children: (
    <a lang="en" className="p-name u-url u-uid" href="https://astrid.tech">
      Astrid Yu
    </a>
  ),
};

export const chinese: HCardProperty = {
  key: "name-zh",
  title: <span lang="zh">名字</span>,
  children: (
    <span lang="zh" className="p-name">
      余茂琦
    </span>
  ),
};

export const birthday: HCardProperty = {
  key: "birthday",
  title: (
    <>
      <FaBirthdayCake title="Birthday" /> Birthday
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
  key: "timezone",
  title: "Timezone",
  children: (
    <data className="p-tz" value="-0700">
      PDT (UTC-7:00)
    </data>
  ),
};

export const pronouns: HCardProperty = {
  key: "pronouns",
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
      <a href="https://pronoun.is/she" className="u-pronoun">
        she/her
      </a>
    </>
  ),
};

export const email: HCardProperty = {
  key: "export",
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

export const linux: HCardProperty = {
  key: "operating-system",
  title: (
    <>
      <FiDisc title="OS" /> OS
    </>
  ),
  children: (
    <>
      <a className="u-os" href="https://nixos.org">
        Nix
        <SiNixos title="O" />
        <span style={{ fontSize: 0 }}>O</span>S
      </a> btw
    </>
  ),
};

export const hobbies: HCardProperty = {
  key: "hobbies",
  title: (
    <>
      <FaDice title="Hobbies" /> Hobbies
    </>
  ),
  children: (
    <>Programming, playing video games, reading fantasy books, watching anime</>
  ),
};

export const vcard: HCardProperty = {
  key: "vcard",
  title: (
    <>
      <FaAddressCard title="vCard" /> vCard
    </>
  ),
  children: <a href="/astridyu.vcard">Add me to your contacts!</a>,
};

// All identities go below this line

export const website: HCardProperty = {
  key: "website",
  title: (
    <>
      <FaGlobe title="Website" /> Website
    </>
  ),
  children: (
    <a href="https://astrid.tech" rel="me" className="u-url">
      astrid.tech
    </a>
  ),
};

export const github: HCardProperty = {
  key: "github",
  title: (
    <>
      <GoMarkGithub title="Github" /> GitHub
    </>
  ),
  children: (
    <a className="u-url" rel="me" href="https://github.com/astridyu">
      astridyu
    </a>
  ),
};

export const hackaday: HCardProperty = {
  key: "hackaday",
  title: (
    <>
      <SiHackaday title="Hackaday" /> Hackaday
    </>
  ),
  children: (
    <a className="u-url" rel="me" href="https://hackaday.io/astralbijection">
      Astrid Yu
    </a>
  ),
};

export const instagram: HCardProperty = {
  key: "instagram",
  title: (
    <>
      <FaInstagram title="Instagram" /> Instagram
    </>
  ),
  children: (
    <a
      className="u-url"
      rel="me"
      href="https://www.instagram.com/astral.bijection"
    >
      astral.bijection
    </a>
  ),
};

export const matrix: HCardProperty = {
  key: "matrix",
  title: (
    <>
      <SiMatrix title="Matrix" /> Matrix
    </>
  ),
  children: (
    <a
      className="u-url"
      rel="me"
      href="https://matrix.to/#/@astralbijection:matrix.org"
    >
      @astralbijection:matrix.org
    </a>
  ),
};

export const reddit: HCardProperty = {
  key: "reddit",
  title: (
    <>
      <FaReddit title="Reddit" /> Reddit
    </>
  ),
  children: (
    <a
      className="u-url"
      rel="me"
      href="https://www.reddit.com/user/astralbijection"
    >
      u/astralbijection
    </a>
  ),
};

export const thingiverse: HCardProperty = {
  key: "thingiverse",
  title: (
    <>
      <FaCubes title="Thingiverse" /> Thingiverse
    </>
  ),
  children: (
    <a
      className="u-url"
      rel="me"
      href="https://www.thingiverse.com/astralbijection"
    >
      Thingiverse
    </a>
  ),
};

export const twitter: HCardProperty = {
  key: "twitter",
  title: (
    <>
      <FaTwitter title="Twitter" /> Twitter
    </>
  ),
  children: (
    <a className="u-url" rel="me" href="https://twitter.com/astralbijection">
      @astralbijection
    </a>
  ),
};
