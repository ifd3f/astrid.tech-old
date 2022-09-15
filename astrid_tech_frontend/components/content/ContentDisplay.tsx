import { FC, ReactElement } from "react";
import * as React from "react";
import rehype from "rehype-parse";
import rehype2react from "rehype-react";
import unified from "unified";
import { ContentImage } from "./ContentImage";
import classNames from "classnames";

const processor = unified()
  .use(rehype, { fragment: true })
  .use(rehype2react, {
    createElement: React.createElement,
    allowDangerousHtml: true,
    components: { img: ContentImage },
  } as any);

type ContentDisplayProps = {
  children: string;
  className?: string;
};

export const ContentDisplay: FC<ContentDisplayProps> = ({
  children,
  className,
}) => {
  return (
    <article className={classNames("longform e-content", className)}>
      {processor.processSync(children).result as ReactElement}
    </article>
  );
};
