import React from 'react';
import logo from './logo.svg';
import {BrowserRouter, Switch, Route} from "react-router-dom";
import './App.css';
import MainNavbar from './mainnavbar/MainNavbar';
import Homepage from './homepage/Homepage';

function App() {
  return (
    <BrowserRouter>
      <MainNavbar />
      <Switch>
        <Route path="/"><Homepage/></Route>
      </Switch>
    </BrowserRouter>
  );
}

export default App;
