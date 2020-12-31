import moment from "moment";
import Link from "next/link";
import React, { FC } from "react";
import { Row } from "reactstrap";
import { blogSlugToString, getBlogSlug } from "../../lib/util";
import style from "../../styles/blog.module.scss";
import { BlogPostMeta } from "../../types/types";
import { TagList } from "../tags/tag";

type PostProps = {
  post: BlogPostMeta<Date>;
};

export const PostBrief: FC<PostProps> = ({ post }) => {
  const dateString = moment(post.date).format("DD MMMM YYYY");
  const url = blogSlugToString(getBlogSlug(post));

  return (
    <Link href={url}>
      <article className={style.brief}>
        <Row>
          <div className="col-12 col-sm-8 col-md-7">
            <h3>{post.title}</h3>
            <p>{post.description}</p>
            <p className="text-muted">{post.excerpt}</p>
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

export type BlogFeedProps = {
  posts: BlogPostMeta<Date>[];
};

export const BlogFeed: FC<BlogFeedProps> = ({ posts }) => {
  return (
    <div>
      {posts.map((post) => (
        <PostBrief key={post.slug} post={post} />
      ))}
      <p className="text-center text-muted">(End of posts)</p>
    </div>
  );
};
