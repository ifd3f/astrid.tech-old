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

  return (
    <article className={classNames(style.brief, "h-entry")}>
      <Row>
        <div className="col-12 col-sm-8 col-md-7">
            {post.title ? <h3 className="p-name">{post.title}</h3> : null}
            {post.description ? (
              <p className="p-summary">{post.description}</p>
            ) : null}
            <p className="p-summary text-muted">
              {post.excerpt} <Link href={url}>read more</Link>
            </p>
        </div>
        <div className="col col-sm-4 col-md-5">
          <p className={classNames("text-muted", style.date)}>
            <SemanticDate
              formatStyle="d MMM yyyy"
              date={post.date}
              className="dt-published"
            />
          </p>
          <p>
            <TagList tags={post.tags} link limit={5} />
          </p>
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
