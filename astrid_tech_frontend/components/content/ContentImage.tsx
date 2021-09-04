import path from 'path';
import React, { ComponentProps, FC } from 'react';

export const ContentImage: FC<ComponentProps<'img'>> = (props) => {
  return (
    <a href={props.src}>
      <img {...props} width={1200} />
    </a>
  );
};
