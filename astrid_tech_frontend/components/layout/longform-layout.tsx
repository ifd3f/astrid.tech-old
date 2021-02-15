import SEO from "components/seo";
import path from "path";
import React, { FC, PropsWithChildren, ReactNode } from "react";
import { MdComment } from "react-icons/md";
import { Col, Container, Row } from "reactstrap";
import { TagList } from "../tags/tag";
import style from "./longform-layout.module.scss";
import { PageHeading } from "./page-heading";

type LongformLayoutProps = {
  title: string;
  description: ReactNode;
  descriptionRaw: string;
  headingColor: string;
  above?: ReactNode;
  thumbnail?: string;
  sidebar: ReactNode;
  children: ReactNode;
  url?: string;
};

export const LongformLayout: FC<LongformLayoutProps> = ({
  title,
  description, 
  descriptionRaw,
  headingColor,
  above,
  sidebar,
  thumbnail,
  children,
  url,
}) => {
  return (
    <>
      <SEO
        canonicalUrl={url}
        title={title!}
        description={descriptionRaw}
        image={thumbnail}
      />
      <PageHeading
        above={above}
        title={title}
        description={description}
        bgColor={headingColor}
      />
      <Container className={style.container}>
        <Row>
          <Col lg={8} className={style.content}>
            {children}
          </Col>
          <Col lg={4} className={style.sidebar}>
            {sidebar}
          </Col>
        </Row>
      </Container>
    </>
  );
};

export const SidebarGroup: FC<PropsWithChildren<{}>> = ({ children }) => (
  <div className={style.sidebarGroup}>{children}</div>
);

type InfoRowProps = {
  name: string;
  icon?: ReactNode;
  show?: any;
  children: ReactNode;
};

export const InfoRow: FC<InfoRowProps> = ({
  name,
  icon,
  children,
  show = true,
}) =>
  show ? (
    <tr>
      <th style={{ minWidth: 180 }}>
        {icon ? <>{icon} </> : null}
        {name}
      </th>
      <td className={style.statusData}>{children}</td>
    </tr>
  ) : null;

type StatusGroupProps = {
  children: ReactNode;
};

export const StatusGroup: FC<StatusGroupProps> = ({ children }) => (
  <SidebarGroup>
    <table style={{ width: "100%" }}>
      <tbody>{children}</tbody>
    </table>
  </SidebarGroup>
);

type TagsGroupProps = {
  tags: string[];
};

export const TagsGroup: FC<TagsGroupProps> = ({ tags }) => {
  return (
    <SidebarGroup>
      <h2>Tags</h2>
      <TagList tags={tags} link />
    </SidebarGroup>
  );
};

export const CommentsRow = ({ count }: { count: number }) => {
  return (
    <InfoRow name="Comments" icon={<MdComment />}>
      <a href="#comments">{count}</a>
    </InfoRow>
  );
};
