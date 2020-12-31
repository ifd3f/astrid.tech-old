import Image from "next/image";
import React, { FC } from "react";

export const ContentImage: FC<{ src: string; alt: string }> = ({
  src,
  alt,
}) => {
  return (
    <a href={src}>
      <Image
        src={src}
        alt={alt}
        width={1200}
        height={800}
        layout="responsive"
        objectFit="contain"
      />
    </a>
  );
};
