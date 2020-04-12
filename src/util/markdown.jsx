import React from "react";

import ReactMarkdown from "react-markdown";
import MathJax from "react-mathjax";
import RemarkMathPlugin from "remark-math";

/**
 * Render markdown with MathJax
 *
 * @see {@link https://medium.com/@MatDrinksTea/rendering-markdown-and-latex-in-react-dec355e74119|this post} for further information
 */
export default function MarkdownRender(props) {
  const newProps = {
    ...props,
    plugins: [RemarkMathPlugin],
    renderers: {
      ...props.renderers,
      math: (props) => <MathJax.Node formula={props.value} />,
      inlineMath: (props) => <MathJax.Node inline formula={props.value} />,
    },
  };
  return (
    <MathJax.Provider input="tex">
      <ReactMarkdown {...newProps} />
    </MathJax.Provider>
  );
}
