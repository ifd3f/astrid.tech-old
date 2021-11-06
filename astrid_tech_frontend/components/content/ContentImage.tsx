import { ComponentProps, FC } from "react";

export const ContentImage: FC<ComponentProps<"img">> = (props) => {
  return (
    <div style={{ textAlign: "center", marginBottom: 10 }}>
      <img
        {...props}
        title={props.alt}
        style={{ maxHeight: "600px", maxWidth: "100%" }}
      />
    </div>
  );
};
