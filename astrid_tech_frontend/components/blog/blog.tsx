require(`katex/dist/katex.min.css`);
import { graphql } from "gatsby";
import React, { createContext, FC, useContext } from "react";
import { FaCalendar } from "react-icons/fa";
import { Container } from "reactstrap";
import { LongformLayout, StatusGroup } from "src/components/layout";
import {
  CommentsRow,
  InfoRow,
  TagsGroup,
} from "src/components/layout/longform-layout";
import { getHSLString, getPersistentColor } from "src/util";
import { CommentSection } from "../components/api/comments/CommentSection";
import Layout from "../components/layout/layout";
import { BlogPost } from "../types/index";

export const pageQuery = graphql`
  query BlogPostBySlug($slug: String!) {
    blogPost(slug: { eq: $slug }) {
      description
      source {
        html
      }
      thumbnail {
        childImageSharp {
          fixed(width: 1200, height: 630, toFormat: PNG, cropFocus: CENTER) {
            src
          }
        }
      }
      title
      date(formatString: "YYYY MMMM DD")
      slug
      tags {
        ...TagBadge
      }
    }
  }
`;

type Data = {
  blogPost: BlogPost;
};

type Context = {
  previous: BlogPost;
  next: BlogPost;
};

type PostContextData = {
  post: BlogPost;
  disqusConfig: any;
};

const ProjectContext = createContext<PostContextData>({} as PostContextData);

const PostStatusGroup: FC = () => {
  const { post, disqusConfig } = useContext(ProjectContext);
  return (
    <StatusGroup>
      <InfoRow name="Date" icon={<FaCalendar />}>
        {post.date}
      </InfoRow>
      <CommentsRow disqusConfig={disqusConfig} />
    </StatusGroup>
  );
};

export type BlogPostPageProps = { post: BlogPost<string> };

export const BlogPostPage: FC<BlogPostPageProps> = ({ post }) => {
  const url = `${location.origin}${post.slug}`;
  const disqusConfig = {
    url,
    identifier: post.slug,
    title: post.title,
  };

  return (
    <ProjectContext.Provider value={{ post, disqusConfig }}>
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
            dangerouslySetInnerHTML={{ __html: post.source.html }}
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
