import { CSSProperties, FC, ReactNode } from "react";
import styles from "./style.module.scss";

export type HomepageSectionProps = {
  children: ReactNode;
  style?: CSSProperties;
};

export const HomepageSection: FC<HomepageSectionProps> = ({
  children,
  style,
}) => {
  return (
    <section style={style}>
      <div className={styles.sectionContent}>{children}</div>
    </section>
  );
};
