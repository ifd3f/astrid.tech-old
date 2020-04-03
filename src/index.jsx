/* eslint-disable */

import React from "react";
import { hydrate, render } from "react-dom";
import App from "./App";
import * as serviceWorker from "./serviceWorker";

const root = document.getElementById("root");
if (root.hasChildNodes()) {
  hydrate(
    <React.StrictMode>
      <App />
    </React.StrictMode>,
    root
  );
} else {
  render(
    <React.StrictMode>
      <App />
    </React.StrictMode>,
    root
  );
}

// If you want your app to work offline and load faster, you can change
// unregister() to register() below. Note this comes with some pitfalls.
// Learn more about service workers: https://bit.ly/CRA-PWA
serviceWorker.unregister();
