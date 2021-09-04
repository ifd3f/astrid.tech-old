import React, { FC } from 'react';
import { BsEnvelope } from 'react-icons/bs';
import { GiPhone } from 'react-icons/gi';
import { GoMarkGithub } from 'react-icons/go';
import { Container } from 'reactstrap';
import Layout from '../components/layout/layout';
import SEO from '../components/seo';
import styles from '../styles/blog.module.scss';

type Data = {
  file: { childMarkdownRemark: { html: string } };
};

function Bio() {
  return (
    <div>
      <h1>About me</h1>
      <p>
        Hey, I'm Astrid and I'm a programmer. I got into coding when I was
        around 12, back when I played a ton of Minecraft. There was this mod for
        the game called{' '}
        <a href="http://www.computercraft.info/">ComputerCraft</a> that added
        computers to the game. I thought that was really cool, so I ended up
        teaching myself Lua to program those computers.
      </p>
      <p>
        Later on, I started branching out into more and more languages,
        technologies, and projects, and I eventually ended up with the
        7-year-long-and-counting mess that you can see on the{' '}
        <a href="/projects">projects</a>. I wanted a place to share what I've
        made, so I created this website!
      </p>
    </div>
  );
}
const About: FC<{}> = (props) => {
  // TODO add linkedin when... well, you know
  return (
    <Layout {...props} currentLocation="about">
      <SEO
        title="About"
        description="Information about the not-particularly-illustrious person known as Astrid Yu."
      />
      <Container className={styles.blogContentContainer}>
        <Bio />
        <div>
          <ul>
            <li>
              <a href="mailto:astrid@astrid.tech">
                <BsEnvelope title="Email" /> astrid@astrid.tech
              </a>
            </li>
            <li>
              <a href="tel:+18052705368">
                <GiPhone title="Phone" /> ‪(805) 270-5368‬
              </a>
            </li>
            <li>
              <a href="https://github.com/astralbijection">
                <GoMarkGithub title="GitHub" /> Follow me on GitHub
              </a>
            </li>
          </ul>
        </div>
      </Container>
    </Layout>
  );
};

export default About;
