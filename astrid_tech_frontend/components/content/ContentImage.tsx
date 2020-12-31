import Image from "next/image";
import React, { ComponentProps, FC } from "react";

export const ContentImage: FC<ComponentProps<"img">> = ({ src, alt }) => {
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
