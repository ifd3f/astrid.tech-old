import "bootstrap/dist/css/bootstrap.min.css";
import React from "react";
import "react-responsive-carousel/lib/styles/carousel.min.css";
import { BrowserRouter, Route, Switch, Redirect } from "react-router-dom";
import "./App.css";
import "./bootstrap.scss";
import Homepage from "./homepage";
import MainNavbar from "./mainnavbar/MainNavbar";
import Page404 from "./404";
import BlogPage from "./blog";
import MainFooter from "./footer";
import PortfolioPage from "./portfolio";

function App() {
  return (
    <BrowserRouter>
      <MainNavbar />
      <div>
        <Switch>
          <Route path="/" exact>
            <Homepage />
          </Route>
          <Route path="/blog">
            <BlogPage />
          </Route>
          <Route path="/works">
            <PortfolioPage />
          </Route>
          <Route path="/404" exact>
            <Page404 />
          </Route>
          <Route path="*">
            <Redirect to="/404" />
          </Route>
        </Switch>
      </div>
      <MainFooter />
    </BrowserRouter>
  );
}

export default App;
