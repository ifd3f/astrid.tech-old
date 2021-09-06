import { FC, ReactElement } from "react";
import * as React from "react";
import rehype from "rehype-parse";
import rehype2react from "rehype-react";
import unified from "unified";
import { ContentImage } from "./ContentImage";

const processor = unified()
  .use(rehype, { fragment: true })
  .use(rehype2react, {
    createElement: React.createElement,
    allowDangerousHtml: true,
    components: { img: ContentImage },
  } as any);

export const ContentDisplay: FC<{ children: string }> = ({ children }) => {
  return (
    <article className="longform e-content">
      {processor.processSync(children).result as ReactElement}
    </article>
  );
};
