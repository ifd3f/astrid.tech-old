import { format } from "date-fns";
import { Path } from "lib/cache";
import { join } from "path";
import React, { createContext, FC, useContext } from "react";
import { FaCalendar } from "react-icons/fa";
import { Container } from "reactstrap";
import {
  blogSlugToString,
  getBlogSlug,
  getHSLString,
  getPersistentColor,
} from "../../lib/util";
import { BlogPost } from "../../types/types";
import { CommentSection } from "../api/comments/CommentSection";
import { ContentDisplay } from "../content";
import Layout from "../layout/layout";
import {
  InfoRow,
  LongformLayout,
  StatusGroup,
  TagsGroup,
} from "../layout/longform-layout";
import SEO from "../seo";

type PostContextData = {
  post: BlogPost<Date>;
};

const ProjectContext = createContext<PostContextData>({} as PostContextData);

const PostStatusGroup: FC = () => {
  const { post } = useContext(ProjectContext);
  const date = format(post.date, "d MMM yyyy");
  return (
    <StatusGroup>
      <InfoRow name="Date" icon={<FaCalendar />}>
        {date}
      </InfoRow>
      {/* TODO add comment count */}
    </StatusGroup>
  );
};

export type ClientBlogPost = {
  title?: string;
  description?: string;
  tags: string[];
  path: Path;
  htmlContent?: string;
};

export type BlogPostPageProps = { post: ClientBlogPost };

export const BlogPostPage: FC<BlogPostPageProps> = ({ post }) => {
  return (
    <>
      <SEO title={post.title} description={post.description} />
      <Layout>
        <Container>
          <article style={{ backgroundColor: "white" }}>
            {post.title ? <h1>{post.title}</h1> : null}
            {post.description ? <p>{post.description}</p> : null}
            {post.htmlContent ? (
              <div dangerouslySetInnerHTML={{ __html: post.htmlContent }} />
            ) : null}
          </article>
          <section id="comments">
            <h2>Comments</h2>
            <CommentSection slug={"TODO" /*TODO*/} />
          </section>
        </Container>
      </Layout>
    </>
  );
};

export default BlogPostPage;
