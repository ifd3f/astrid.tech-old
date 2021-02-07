import path from "path";
import React, { ComponentProps, FC } from "react";

export const ContentImage: FC<ComponentProps<"img">> = (props) => {
  const { src, alt } = props;
  let url: URL;
  try {
    url = new URL(src!);
  } catch (e) {
    const ext = path.extname(src!);
    if (![".jpg", ".jpeg", ".png", ".webp", ".gif"].includes(ext))
      return <img {...props} />;

    const imgPath = path.join("../../public", src!)
    const srcSet = require(imgPath + "?resize&sizes[]=300&sizes[]=500&sizes[]=800");
    return <img {...props} srcSet={srcSet.srcSet} src={srcSet.src} />;
  }
  return (
    <a href={src}>
      <img src={src!} alt={alt} width={1200} />
    </a>
  );
};
