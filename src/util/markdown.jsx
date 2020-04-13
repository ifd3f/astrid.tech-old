import React, { useState } from "react";

import ReactMarkdown from "react-markdown";
import MathJax from "react-mathjax";
import RemarkMathPlugin from "remark-math";
import Lowlight from "react-lowlight";
import js from "highlight.js/lib/languages/javascript";

Lowlight.registerLanguage("js", js);

export function MarkdownRenderAsync({ location, children = null }) {
  const [active, setActive] = useState(false);
  const [source, setSource] = useState(null);

  if (location) {
    fetch(location)
      .then((response) => response.text())
      .then((source) => {
        setSource(source);
        setActive(true);
      });
  }

  return active ? <MarkdownRender source={source} /> : children;
}

/**
 * Render markdown with MathJax
 *
 * @see {@link https://medium.com/@MatDrinksTea/rendering-markdown-and-latex-in-react-dec355e74119|this post} for further information
 */
export function MarkdownRender(props) {
  const newProps = {
    ...props,
    plugins: [RemarkMathPlugin],
    renderers: {
      ...props.renderers,
      math: (props) => <MathJax.Node formula={props.value} />,
      inlineMath: (props) => <MathJax.Node inline formula={props.value} />,
      Code: Lowlight,
    },
  };
  return (
    <MathJax.Provider input="tex">
      <ReactMarkdown {...newProps} />
    </MathJax.Provider>
  );
}
