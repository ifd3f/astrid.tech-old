import React, { FC, ReactElement } from "react";
import rehype from "rehype-parse";
import rehype2react from "rehype-react";
import unified from "unified";
import { ContentImage } from "./ContentImage";

const processor = unified()
  .use(rehype, { fragment: true })
  .use(rehype2react, {
    createElement: React.createElement,
    components: { img: ContentImage },
  });

export const ContentDisplay: FC<{ children: string }> = ({ children }) => {
  const result = processor.processSync(children).result as ReactElement;
  return result;
};
