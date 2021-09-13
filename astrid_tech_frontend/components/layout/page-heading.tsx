import { FC, ReactNode } from "react";
import styles from "./page-heading.module.scss";

type PageHeadingProps = {
  title?: string;
  className?: string;
  description?: ReactNode;
  textColor?: string;
  bgColor?: string;
  above?: ReactNode;
  children?: ReactNode;
};

export const PageHeading: FC<PageHeadingProps> = ({
  title,
  className,
  description,
  bgColor,
  textColor,
  above,
  children,
}) => {
  return (
    <header
      className={className}
      style={{ backgroundColor: bgColor, color: textColor }}
    >
      {above ? <nav className={styles.above}>{above}</nav> : null}
      <div className={styles.header}>
        {title ? <h1 className="p-name">{title}</h1> : null}
        {description ? <p className="p-summary">{description}</p> : null}
        {children}
      </div>
    </header>
  );
};
