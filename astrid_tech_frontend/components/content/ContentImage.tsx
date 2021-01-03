import Image from "next/image";
import React, { ComponentProps, FC } from "react";

export const ContentImage: FC<ComponentProps<"img">> = (props) => {
  const { src, alt } = props;
  let url: URL;
  try {
    url = new URL(src!);
  } catch (e) {
    return <img {...props} />;
  }
  if (url.hostname == "i.ytimg.com") return <img {...props} />;
  return (
    <a href={src}>
      <Image
        src={src!!}
        alt={alt}
        width={1200}
        height={800}
        layout="responsive"
        objectFit="contain"
      />
    </a>
  );
};
