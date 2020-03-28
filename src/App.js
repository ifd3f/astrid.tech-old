import React from "react";
import { BrowserRouter, Route, Switch } from "react-router-dom";
import "./App.css";
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
