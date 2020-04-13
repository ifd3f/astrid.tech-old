import React from "react";
import { MarkdownRenderAsync } from "../util";
import style from "./style.module.css";
import { skills } from "../db";
import { SkillBadge } from "../util";
import { Badge } from "reactstrap";
import { Link } from "react-router-dom";

export interface BlogEntry {
  title: string;
  source: string;
  date: Date;
  tags: Array<string>;
}

function formatDate(d: Date) {
  var month = "" + (d.getMonth() + 1);
  var day = "" + d.getDate();
  var year = d.getFullYear();

  if (month.length < 2) month = "0" + month;
  if (day.length < 2) day = "0" + day;

  return [year, month, day].join("-");
}

interface ArticleTagProps {
  tag: string;
}

export const ArticleTag: React.SFC<ArticleTagProps> = ({ tag }) => {
  if (!tag) {
    return null;
  }
  if (tag[0] == ":") {
    const skillId = tag.substring(1);
    const skill = skills.get(skillId);
    return <SkillBadge skill={skill} />;
  }
  return (
    <Badge tag={Link} to={`/blog/tag/${tag}`}>
      {tag}
    </Badge>
  );
};

interface BlogEntryRendererProps {
  entry: BlogEntry;
}

export const BlogEntryRenderer: React.SFC<BlogEntryRendererProps> = ({
  entry,
}) => {
  const tags = entry.tags.map((tag) => <ArticleTag tag={tag} />);
  return (
    <article className="blogArticle">
      <h1>{entry.title}</h1>
      <p>published on {formatDate(entry.date)}</p>
      <p>Tags: {tags}</p>
      <MarkdownRenderAsync location={entry.source} />
    </article>
  );
};
