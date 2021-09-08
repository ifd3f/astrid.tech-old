import classNames from "classnames";
import { FC, ReactNode, useState } from "react";
import * as React from "react";
import { BsCaretLeftFill } from "react-icons/bs";
import { Badge } from "reactstrap";
import { Tag } from "../../types/types";
import style from "./tag.module.scss";
import { useTagTable } from "./TagTableProvider";
import Link from "next/link";

type TagBadgeProps = {
  tag: Tag | string;
  link?: boolean;
  children?: ReactNode;
  relTag?: boolean;
};

export const TagBadge: FC<TagBadgeProps> = ({
  tag,
  link = false,
  children,
  relTag = false,
}) => {
  if (typeof tag == "string") {
    const table = useTagTable();
    tag = table.get(tag);
  }
  const getLinkTo = (slug: string) =>
    process.env.publicRoot + (slug[0] == "/" ? slug : "/t/" + slug);

  const badge = (
    <Badge
      rel={relTag ? "tag" : undefined}
      className={classNames(style.tag, "p-category")}
      style={{
        backgroundColor: tag.backgroundColor,
        color: tag.color,
        cursor: "pointer",
      }}
    >
      {tag.name}
      {children}
    </Badge>
  );

  return link ? (
    <Link href={getLinkTo(tag.slug)} passHref>
      {badge}
    </Link>
  ) : (
    badge
  );
};

type TagListProps = {
  tags: string[];
  limit?: number;
  link?: boolean;
  className?: string;
  relTag?: boolean;
};

export const TagList: FC<TagListProps> = ({
  tags,
  link = false,
  limit = Number.MAX_SAFE_INTEGER,
  className,
  relTag,
}) => {
  const [isOpened, setOpened] = useState(false);

  const excluded = tags.slice(limit);

  const onClick = (ev: React.MouseEvent<HTMLElement, MouseEvent>) => {
    ev.preventDefault();
    ev.stopPropagation();
    setOpened(!isOpened);
  };

  const alt = isOpened
    ? "Close"
    : "Click to show: " + excluded.map((tag) => tag).join(", ");
  const shownTags = isOpened ? tags : tags.slice(0, limit);
  const openBadgeText = isOpened ? (
    <BsCaretLeftFill />
  ) : (
    <>+{excluded.length}</>
  );

  return (
    <div className={className}>
      <p
        style={{
          fontSize: "12pt",
          marginBottom: 3,
        }}
      >
        {shownTags.map((tag) => (
          <>
            <TagBadge key={tag} tag={tag} link={link} relTag={relTag} />{" "}
          </>
        ))}
        {excluded.length > 0 ? (
          <Badge title={alt} onClick={onClick} style={{ cursor: "pointer" }}>
            {openBadgeText}
          </Badge>
        ) : null}
      </p>
    </div>
  );
};
