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

export function FooterSection() {
  return (
    <footer style={{ background: "#202030" }}>
      <div className="row">
        <div className="col-lg-12">
          <p>
            Made with <Tea /> and <Witch /> by Astrid Augusta Yu.
          </p>
        </div>
      </div>
    </footer>
  );
}
