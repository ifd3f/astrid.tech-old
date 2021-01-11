import { ComponentProps, FC } from "react";

export const ContentIFrame: FC<ComponentProps<"iframe">> = ({ ...props }) => {
  return (
    <div>
      <iframe {...props} />
    </div>
  );
};
