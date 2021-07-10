import classNames from "classnames";
import React, { FC, ReactNode, useState } from "react";
import { BsCaretLeftFill } from "react-icons/bs";
import { Badge } from "reactstrap";
import { Tag } from "../../types/types";
import { ALink } from "../util/boilerplate";
import style from "./tag.module.scss";
import { useTagTable } from "./TagTableProvider";

type TagBadgeProps = {
  tag: Tag;
  link?: boolean;
  children?: ReactNode;
};

export const TagBadge: FC<TagBadgeProps> = ({
  tag,
  link = false,
  children,
}) => {
  const linkTo = tag.id[0] == "/" ? tag.id : "/t/" + tag.id;

  const badge = (
    <Badge
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

  return link ? <ALink href={linkTo}>{badge}</ALink> : badge;
};

type TagListProps = {
  tags: string[];
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
  const { table } = useTagTable();
  const [isOpened, setOpened] = useState(false);

  const excluded = tags.slice(limit);

  // Intercept events before they go down
  const stopPropagation = (ev: React.MouseEvent<HTMLElement, MouseEvent>) => {
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
        {shownTags.map((id) => (
          <TagBadge key={id} tag={table.get(id)} link={link} />
        ))}{" "}
        {excluded.length > 0 ? (
          <Badge title={alt} onClick={stopPropagation} style={{ cursor: "pointer" }}>
            {openBadgeText}
          </Badge>
        ) : null}
      </p>
    </div>
  );
};
