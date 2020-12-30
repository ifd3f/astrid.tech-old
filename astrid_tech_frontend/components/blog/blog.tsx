import { useRouter } from "next/router";
import React, { createContext, FC, useContext } from "react";
import { FaCalendar } from "react-icons/fa";
import { Container } from "reactstrap";
import { getHSLString, getPersistentColor } from "../../lib/util";
import { BlogPost } from "../../types/types";
import { CommentSection } from "../api/comments/CommentSection";
import Layout from "../layout/layout";
import {
  InfoRow,
  LongformLayout,
  StatusGroup,
  TagsGroup,
} from "../layout/longform-layout";

type PostContextData = {
  post: BlogPost<string>;
};

const ProjectContext = createContext<PostContextData>({} as PostContextData);

const PostStatusGroup: FC = () => {
  const { post } = useContext(ProjectContext);
  return (
    <StatusGroup>
      <InfoRow name="Date" icon={<FaCalendar />}>
        {post.date}
      </InfoRow>
      {/* TODO add comment count */}
    </StatusGroup>
  );
};

export type BlogPostPageProps = { post: BlogPost<string> };

export const BlogPostPage: FC<BlogPostPageProps> = ({ post }) => {
  const router = useRouter();
  const url = ""; // TODO `${router.}${post.slug}`;

  return (
    <ProjectContext.Provider value={{ post }}>
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
          <article
            className="longform"
            dangerouslySetInnerHTML={{ __html: post.content }}
          />
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
