import React from "react";
import { MarkdownRenderAsync } from "../util";
import style from "./style.module.css";

export interface BlogEntry {
  title: string;
  source: string;
  date: Date;
}

interface BlogEntryRendererProps {
  entry: BlogEntry;
}

function formatDate(d: Date) {
  var month = "" + (d.getMonth() + 1);
  var day = "" + d.getDate();
  var year = d.getFullYear();

  if (month.length < 2) month = "0" + month;
  if (day.length < 2) day = "0" + day;

  return [year, month, day].join("-");
}

export const BlogEntryRenderer: React.SFC<BlogEntryRendererProps> = ({
  entry,
}) => {
  return (
    <article className="blogArticle">
      <h1>{entry.title}</h1>
      <p>published on {formatDate(entry.date)}</p>
      <MarkdownRenderAsync location={entry.source} />
    </article>
  );
};
