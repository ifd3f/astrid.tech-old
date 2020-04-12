import React from "react";

function Tea() {
  return (
    <span aria-label="tea" role="img">
      ☕
    </span>
  );
}

function Witch() {
  return (
    <span aria-label="witchcraft" role="img">
      🧙‍
    </span>
  );
}

function CreativeCommons() {
  return (
    <>
      <a rel="license" href="http://creativecommons.org/licenses/by/4.0/">
        <img
          alt="Creative Commons License"
          style={{ borderWidth: 0 }}
          src="https://i.creativecommons.org/l/by/4.0/88x31.png"
        />
      </a>
      <br />
      This work is licensed under a{" "}
      <a rel="license" href="http://creativecommons.org/licenses/by/4.0/">
        Creative Commons Attribution 4.0 International License
      </a>
      .
    </>
  );
}

function FooterSection() {
  return (
    <footer style={{ background: "#202030" }}>
      <div className="row">
        <div className="col-lg-12">
          <p>
            Created by Astrid Augusta Yu with <Tea /> and <Witch />
          </p>
          <CreativeCommons />
        </div>
      </div>
    </footer>
  );
}

export default FooterSection;
