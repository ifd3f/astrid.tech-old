import React from "react";
import { Root, Routes, addPrefetchExcludes } from "react-static";
import { Link, Router } from "@reach/router";
import FancyDiv from "components/FancyDiv";
import "./app.css";

// Any routes that start with 'dynamic' will be treated as non-static routes
//addPrefetchExcludes(["dynamic"]);

function App() {
  return (
    <Root>
      <nav>
        <Link to="/">Home</Link>
        <Link to="/about">About</Link>
        <Link to="/blog">Blog</Link>
      </nav>
      <div className="content">
        <FancyDiv>
          <React.Suspense fallback={<em>Loading...</em>}>
            <Router>
              <Routes path="*" />
            </Router>
          </React.Suspense>
        </FancyDiv>
      </div>
    </Root>
  );
}

export default App;
