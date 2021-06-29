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
    <ProjectContext.Provider value={{ post }}>
      <SEO title={"title" /* TODO*/} description={"desc" /* TODO*/} />
      <Layout currentLocation="blog">
        <LongformLayout
          title={"title" /* TODO*/}
          url={""}
          description={"desc" /* TODO*/}
          descriptionRaw={"desc" /* TODO*/}
          headingColor="#FFFFFF"
          sidebar={
            <>
              <PostStatusGroup />
              <TagsGroup tags={post.tags} />
            </>
          }
          above={null}
        >
          {post.htmlContent ? (
            <article className="longform">
              <ContentDisplay>{post.htmlContent}</ContentDisplay>
            </article>
          ) : null}
        </LongformLayout>
        <Container>
          <section id="comments">
            <h2>Comments</h2>
            <CommentSection slug={"TODO" /*TODO*/} />
          </section>
        </Container>
      </Layout>
    </ProjectContext.Provider>
  );
};

export default BlogPostPage;
