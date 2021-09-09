import { ReactNode } from "react";
import { BsEnvelope } from "react-icons/bs";
import {
  FaBirthdayCake,
  FaCubes,
  FaFacebook,
  FaGlobe,
  FaInstagram,
  FaLinkedin,
  FaMastodon,
  FaReddit,
  FaTwitter,
} from "react-icons/fa";
import { GiPhone } from "react-icons/gi";
import { FiDisc } from "react-icons/fi";
import { SiArchlinux, SiNixos, SiHackaday, SiMatrix } from "react-icons/si";
import { GoMarkGithub } from "react-icons/go";

export type HCardProperty = {
  key: string;
  title: ReactNode;
  children: ReactNode;
  classes?: string;
};

export const birthday: HCardProperty = {
  key: "birthday",
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
      <span title='she/her, as in "She went to the park"' className="u-pronoun">
        she/her
      </span>{" "}
      or{" "}
      <span
        title='they/them, as in "They made tacos yesterday"'
        className="u-pronoun"
      >
        they/them
      </span>
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

export const phone: HCardProperty = {
  key: "phone",
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

export const linux: HCardProperty = {
  key: "operating-system",
  title: (
    <>
      <FiDisc title="OS" /> OS
    </>
  ),
  children: (
    <>
      <a className="u-os" href="https://archlinux.org">
        <SiArchlinux title="A" />
        <span style={{ fontSize: 0 }}>A</span>rch btw
      </a>{" "}
      and{" "}
      <a className="u-os" href="https://nixos.org">
        Nix
        <SiNixos title="O" />
        <span style={{ fontSize: 0 }}>O</span>S
      </a>
    </>
  ),
};

// All identities go below this line

export const facebook: HCardProperty = {
  key: "facebook",
  title: (
    <>
      <FaFacebook title="Facebook" /> Facebook
    </>
  ),
  children: (
    <a
      className="u-url"
      rel="me"
      href="https://www.facebook.com/astral.bijection"
    >
      Astrid Yu
    </a>
  ),
};

export const github: HCardProperty = {
  key: "github",
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
      Astrid Yu
    </a>
  ),
};

export const linkedin: HCardProperty = {
  key: "linkedin",
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

export const mastodon: HCardProperty = {
  key: "mastodon",
  title: (
    <>
      <FaMastodon title="Mastodon" /> Mastodon
    </>
  ),
  children: (
    <a className="u-url" rel="me" href="https://tech.lgbt/@astralbijection">
      @astralbijection@tech.lgbt
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

export const indieweb: HCardProperty = {
  key: "indieweb",
  title: (
    <>
      <FaGlobe title="IndieWeb" /> IndieWeb
    </>
  ),
  children: (
    <a className="u-url" rel="me" href="https://indieweb.org/User:Astrid.tech">
      astrid.tech
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
