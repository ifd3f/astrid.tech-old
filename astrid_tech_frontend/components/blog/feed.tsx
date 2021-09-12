import classNames from "classnames";
import { SemanticDate } from "components/util/date-displays";
import Link from "next/link";
import { FC } from "react";
import { Row } from "reactstrap";
import { blogSlugToString, getBlogSlug } from "../../lib/util";
import style from "../../styles/blog.module.scss";
import { BlogPostMeta } from "../../types/types";
import { TagList } from "../tags/tag";

type PostProps = {
  post: BlogPostMeta<Date>;
};

export const PostBrief: FC<PostProps> = ({ post }) => {
  const url = blogSlugToString(getBlogSlug(post));

  const title = post.title ? <h3 className="p-name">{post.title}</h3> : null;
  const description = post.description ? (
    <p className="p-summary">{post.description}</p>
  ) : null;
  const body = (
    <p className={classNames("p-summary", post.title ? "text-muted" : null)}>
      {post.excerpt} <Link href={url}>read more</Link>
    </p>
  );
  const date = (
    <p className={classNames("text-muted", style.date)}>
      <SemanticDate
        formatStyle="d MMM yyyy"
        date={post.date}
        className="dt-published"
      />
    </p>
  );
  const tags = (
    <p>
      <TagList tags={post.tags} link limit={5} />
    </p>
  );

  return (
    <article className={classNames(style.brief, "h-entry")}>
      <Row>
        <div className="col-12 col-sm-8 col-md-7">
          {title}
          {description}
          {body}
        </div>
        <div className="col col-sm-4 col-md-5">
          {date}
          {tags}
        </div>
      </Row>
    </article>
  );
};

export type BlogFeedProps = {
  posts: BlogPostMeta<Date>[];
};

export const BlogFeed: FC<BlogFeedProps> = ({ posts }) => {
  return (
    <section className="h-feed">
      {posts.map((post) => (
        <PostBrief key={post.slug} post={post} />
      ))}
      <p className="text-center text-muted">(End of posts)</p>
    </section>
  );
};
