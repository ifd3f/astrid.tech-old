import { format } from "date-fns";
import Link from "next/link";
import React, { FC } from "react";
import { Row } from "reactstrap";
import { blogSlugToString, getBlogSlug } from "../../lib/util";
import style from "../../styles/blog.module.scss";
import { BlogPostMeta } from "../../types/types";
import { TagList } from "../tags/tag";

export type PostBriefData = {
  title: string;
  publishedDate: string;
  excerpt: string;
  description: string;
  tags: string[];
  slug: string;
};

export const PostBrief: FC<{ post: PostBriefData }> = ({ post }) => {
  const dateString = format(new Date(post.publishedDate), "d MMMM yyyy");

  return (
    <Link href={post.slug}>
      <article className={style.brief}>
        <Row>
          <div className="col-12 col-sm-8 col-md-7">
            <a href={post.slug}>
              <h3>{post.title}</h3>
              <p>{post.description}</p>
              <p className="text-muted">{post.excerpt}</p>
            </a>
          </div>
          <div className="col col-sm-4 col-md-5">
            <p className={`text-muted ${style.date}`}>{dateString}</p>
            <p>
              <TagList tags={post.tags} link limit={5} />
            </p>
          </div>
        </Row>
      </article>
    </Link>
  );
};

export const BlogFeed: FC<{ posts: PostBriefData[] }> = ({ posts }) => {
  return (
    <div>
      {posts.map((post) => (
        <PostBrief key={post.slug} post={post} />
      ))}
      <p className="text-center text-muted">(End of posts)</p>
    </div>
  );
};
