import Image from "next/image";
import { ComponentProps, FC } from "react";

export const ContentImage: FC<ComponentProps<"img">> = (props) => {
  return (
    <a href={props.src}>
      {/* eslint-disable-next-line @next/next/no-img-element */}
      <img {...props} width={1200} />
    </a>
  );
};
