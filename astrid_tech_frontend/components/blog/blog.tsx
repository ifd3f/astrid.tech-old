import { format } from "date-fns";
import { join } from "path";
import { createContext, FC, useContext } from "react";
import { FaCalendar, FaLink } from "react-icons/fa";
import { Container } from "reactstrap";
import {
  blogSlugToString,
  getBlogShortLinkCode,
  getBlogSlug,
  getHSLString,
  getPersistentColor,
  truncateKeepWords,
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
import striptags from "striptags";
import { SemanticDate } from "components/util/date-displays";

type PostContextData = {
  post: BlogPost<Date>;
};

const ProjectContext = createContext<PostContextData>({} as PostContextData);

const PostStatusGroup: FC = () => {
  const { post } = useContext(ProjectContext);

  const fullSlug = blogSlugToString(getBlogSlug(post));
  const link = process.env.publicRoot!! + fullSlug;

  //const shortcode = getBlogShortLinkCode(post);
  //const shortlink = `https://aay.tw/${shortcode}`;

  return (
    <StatusGroup>
      <InfoRow name="Published" icon={<FaCalendar />}>
        <SemanticDate date={post.date} formatStyle="d MMM yyyy" />
      </InfoRow>
      <InfoRow name="Permalink" icon={<FaLink />}>
        <a href={link} className="u-url u-uid" rel="bookmark">
          {post.slug}
        </a>
      </InfoRow>
      {/* TODO get shortlink working
        <InfoRow name="Shortlink" icon={<FaLink />}>
          <a href={shortlink} className="u-url">
            aay.tw/{shortcode}
          </a>
        </InfoRow>
      */}
      {/* TODO add comment count */}
    </StatusGroup>
  );
};

export type BlogPostPageProps = { post: BlogPost<Date> };

export const BlogPostPage: FC<BlogPostPageProps> = ({ post }) => {
  const slug = blogSlugToString(getBlogSlug(post));
  const url = join(process.env.publicRoot!, slug);

  const metaTitle = post.title
    ? post.title
    : truncateKeepWords(striptags(post.content), 50) + "...";

  return (
    <ProjectContext.Provider value={{ post }}>
      <SEO title={metaTitle} description={post.description} />
      <Layout currentLocation="blog">
        <LongformLayout
          title={post.title}
          url={url}
          description={post.description}
          descriptionRaw={post.description}
          headingColor={getHSLString(getPersistentColor(slug))}
          sidebar={
            <>
              <PostStatusGroup />
              <TagsGroup tags={post.tags} />
            </>
          }
          above={null}
        >
          <ContentDisplay className="h-entry">{post.content}</ContentDisplay>
        </LongformLayout>
        <Container>
          <section id="comments">
            <h2>Comments</h2>
            <CommentSection slug={slug} />
          </section>
        </Container>
      </Layout>
    </ProjectContext.Provider>
  );
};

export default BlogPostPage;
