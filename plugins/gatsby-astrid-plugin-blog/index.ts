export const BLOG_POST_MIME_TYPE = "application/prs.astrid-tech-blog-post"

export type BlogPostContent = {
  title: string
  date: string
  slug: string
  tagSlugs: string[]
  description: string
  content: string
}
