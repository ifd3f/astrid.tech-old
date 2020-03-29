import "bootstrap/dist/css/bootstrap.min.css";
import React from "react";
import "react-responsive-carousel/lib/styles/carousel.min.css";
import { BrowserRouter, Route, Switch } from "react-router-dom";
import "./App.css";
import "./bootstrap.css";
import Homepage from "./homepage/Homepage";
import MainNavbar from "./mainnavbar/MainNavbar";

function App() {
  return (
    <BrowserRouter>
      <MainNavbar />
      <Switch>
        <Route path="/">
          <Homepage />
        </Route>
      </Switch>
    </BrowserRouter>
  );
}

export default App;
