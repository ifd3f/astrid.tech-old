import { format } from "date-fns";
import { useRouter } from "next/router";
import React, { createContext, FC, useContext } from "react";
import { FaCalendar } from "react-icons/fa";
import { Container } from "reactstrap";
import { getHSLString, getPersistentColor } from "../../lib/util";
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

export type BlogPostPageProps = { post: BlogPost<Date> };

export const BlogPostPage: FC<BlogPostPageProps> = ({ post }) => {
  const router = useRouter();
  const url = ""; // TODO `${router.}${post.slug}`;

  return (
    <ProjectContext.Provider value={{ post }}>
      <SEO title={post.title} description={post.description} />
      <Layout currentLocation="blog">
        <LongformLayout
          title={post.title}
          url={url}
          description={post.description}
          descriptionRaw={post.description}
          headingColor={getHSLString(getPersistentColor(post.slug))}
          sidebar={
            <>
              <PostStatusGroup />
              <TagsGroup tags={post.tags} />
            </>
          }
          above={null}
        >
          <article className="longform">
            <ContentDisplay>{post.content}</ContentDisplay>
          </article>
        </LongformLayout>
        <Container>
          <section id="comments">
            <h2>Comments</h2>
            <CommentSection slug={post.slug} />
          </section>
        </Container>
      </Layout>
    </ProjectContext.Provider>
  );
};

export default BlogPostPage;
