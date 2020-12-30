import classNames from "classnames";
import Link from "next/link";
import React, { FC, ReactNode, useState } from "react";
import { BsCaretLeftFill } from "react-icons/bs";
import { Badge } from "reactstrap";
import { Tag } from "../../types/types";
import style from "./tag.module.scss";
import { useTagTable } from "./TagTableProvider";

type TagBadgeProps = {
  tag: Tag | string;
  link?: boolean;
  children?: ReactNode;
};

export const TagBadge: FC<TagBadgeProps> = ({
  tag,
  link = false,
  children,
}) => {
  if (typeof tag == "string") {
    const table = useTagTable();
    tag = table.get(tag);
  }
  const linkTo = tag.slug[0] == "/" ? tag.slug : "/t/" + tag.slug;

  const badge = (
    <Badge
      className={classNames(style.tag, "p-category")}
      tag={"a"}
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

  return link ? <Link href={linkTo}>{badge}</Link> : badge;
};

type TagListProps = {
  tags: (Tag | string)[];
  limit?: number;
  link?: boolean;
  className?: string;
};

export const TagList: FC<TagListProps> = ({
  tags,
  link = false,
  limit = Number.MAX_SAFE_INTEGER,
  className,
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
          <TagBadge key={tag.slug} tag={tag} link={link} />
        ))}{" "}
        {excluded.length > 0 ? (
          <Badge title={alt} onClick={onClick} style={{ cursor: "pointer" }}>
            {openBadgeText}
          </Badge>
        ) : null}
      </p>
    </div>
  );
};
