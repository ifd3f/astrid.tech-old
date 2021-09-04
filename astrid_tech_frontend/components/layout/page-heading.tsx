import React, { FC, ReactNode } from 'react';
import styles from './page-heading.module.scss';

type PageHeadingProps = {
  title: string;
  description?: ReactNode;
  textColor?: string;
  bgColor?: string;
  above?: ReactNode;
};

export const PageHeading: FC<PageHeadingProps> = ({
  title,
  description,
  bgColor,
  textColor,
  above,
}) => {
  return (
    <div style={{ backgroundColor: bgColor }}>
      <nav className={styles.above}>{above}</nav>
      <header className={styles.header}>
        <h1 style={{ color: textColor }}>{title}</h1>
        {description ? <p style={{ color: textColor }}>{description}</p> : null}
      </header>
    </div>
  );
};
