import moment from "moment";
import Link from "next/link";
import React, { FC } from "react";
import { Col, Row } from "reactstrap";
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
        <Row className="col">
          <Col sm={8}>
            <h3>{post.title}</h3>
            <div className="text-muted">{post.description}</div>
          </Col>
          <Col sm={4}>
            <p className={`text-muted ${style.date}`}>{dateString}</p>
            <TagList tags={post.tags} link limit={5} />
          </Col>
        </Row>
        <Col>
          <p>{post.description}</p>
        </Col>
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
