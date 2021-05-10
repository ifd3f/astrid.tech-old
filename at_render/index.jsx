import React from 'react';
import ReactDOMServer from 'react-dom/server';

export function Test() {
    return <a href={"https://google.com"}>Google</a>
}

export function runSSR() {
    return ReactDOMServer.renderToString(<Test/>)
}

runSSR()