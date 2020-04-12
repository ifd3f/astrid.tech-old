import React from "react";
import { Container, Button } from "reactstrap";
import { Link } from "react-router-dom";

export default function Page404() {
  return (
    <Container>
      <h2>404 - There's nothing here!</h2>
      <p>
        Your link seems to be broken, but your code certainly wouldn't be if I
        was on your team!
      </p>
      <Button tag={Link} to="/">
        Find out more about my work
      </Button>
      <p>
        Unless my website directed you here, of course. In that case, please
        tell me what went wrong at{" "}
        <a href="mailto:astridyu3.14@gmail.com">astridyu3.14@gmail.com</a>.
      </p>
      <h3>Where can you go?</h3>
      <ul>
        <li>
          <Link to="/">The homepage</Link> where you can see my face and some
          words about me
        </li>
        <li>
          <Link to="/blog">My blog</Link> where you can see the words in my head
        </li>
        <li>
          <Link to="/works">A directory of my work</Link>
        </li>
      </ul>
    </Container>
  );
}
