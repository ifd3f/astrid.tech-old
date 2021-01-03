import React, { ComponentProps, FC } from "react";

export const ContentImage: FC<ComponentProps<"img">> = (props) => {
  const { src, alt } = props;
  let url: URL;
  try {
    url = new URL(src!);
  } catch (e) {
    return <img {...props} src={require(src!)} />;
  }
  return (
    <a href={src}>
      <img src={src!} alt={alt} width={1200} height={800} />
    </a>
  );
};
