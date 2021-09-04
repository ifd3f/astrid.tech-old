import axios, { AxiosInstance, AxiosResponse } from 'axios';

export type CommentForm = {
  author_name: string | null;
  author_email: string;
  author_website: string | null;
  reply_parent?: number;
  content_md: string;
  slug: string;
};

type CommentData = {
  id: number;
  time_authored: string;
  reply_parent?: number;
  author_name?: string;
  author_email: string;
  author_website?: string;
  content_md: string;
  content_html: string;
  slug: string;
};

export type CommentAuthor = {
  name: string;
  email: string;
  website: string;
};

export type Comment = {
  id: number;
  timeAuthored: Date;
  children: Comment[];
  author: CommentAuthor;
  htmlContent: string;
};

export class AstridTechAPI {
  private axios: AxiosInstance;

  constructor(root: string) {
    this.axios = axios.create({ baseURL: root });
  }

  async createComment(
    comment: CommentForm
  ): Promise<AxiosResponse<CommentData>> {
    return await this.axios.post(`/api/comments/`, comment);
  }

  async reportComment(comment: number, reason: string, email?: string) {
    return await this.axios.post(`/api/comments/${comment}/report/`, {
      reason,
      email,
    });
  }

  async getComments(slug: string): Promise<Comment[]> {
    const response = await this.axios.get<CommentData[]>('/api/comments/', {
      params: { slug },
    });
    const data = response.data;

    const idToComment = new Map(
      data.map((c) => [
        c.id,
        {
          id: c.id,
          timeAuthored: new Date(c.time_authored),
          children: [],
          htmlContent: c.content_html,
          author: {
            name: c.author_name,
            website: c.author_website,
            email: c.author_email,
          },
        } as Comment,
      ])
    );

    const output = [];

    for (let raw of data) {
      const comment = idToComment.get(raw.id)!!;
      if (!raw.reply_parent) {
        output.push(comment);
        continue;
      }
      const parent = idToComment.get(raw.reply_parent)!!;
      parent?.children.push(comment);
    }

    return output;
  }
}
