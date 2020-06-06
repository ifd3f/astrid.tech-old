import React from "react";
import { addPrefetchExcludes, Root } from "react-static";
import "./app.css";

// Any routes that start with 'dynamic' will be treated as non-static routes
addPrefetchExcludes(["dynamic"]);

function App() {
  return (
    <Root>
      <p>hello</p>
    </Root>
  );
}

export default App;
