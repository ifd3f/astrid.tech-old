export type Tagged = {
  tags: string[];
};

export type Tag = {
  name: string;
  color: string;
  backgroundColor: string;
  slug: string;
};

export type WorkExperience = Tagged & {
  organization: string;
  position: string;
  location: string;
  website: string;
  startDate: Date;
  endDate?: Date;
  highlights: string[];
  summary?: string;
};

export type ProjectStatus = null | "early" | "wip" | "complete" | "scrapped";

export type ProjectMeta<DateType = Date> = Tagged & {
  id?: number;
  title: string;
  assetRoot: string;
  status: ProjectStatus;
  startDate: DateType;
  endDate: DateType | null;
  slug: string;
  url: string;
  source: string[];
  thumbnail: string;
  description: null | string;
};

export type Project<DateType = Date> = ProjectMeta<DateType> & {
  content: string;
};

export function convertProjectToObjectDate(project: Project<string>) {
  return {
    ...project,
    startDate: new Date(project.startDate),
    endDate: project.endDate ? new Date(project.endDate) : null,
  };
}

export function convertProjectToStringDate(project: Project<Date>) {
  return {
    ...project,
    startDate: project.startDate.toISOString(),
    endDate: project.endDate ? project.endDate.toISOString() : null,
  };
}

export type BlogPostMeta<DateType = Date> = Tagged & {
  title: string;
  assetRoot: string;
  description: string;
  date: DateType;
  slug: string;
  thumbnail: string | null;
  excerpt?: string;
};

export type BlogPost<DateType = Date> = BlogPostMeta<DateType> & {
  content: string;
};

export function convertBlogPostToObjectDate(post: BlogPostMeta<string>) {
  return {
    ...post,
    date: new Date(post.date),
  };
}

export function convertBlogPostToStringDate(post: BlogPostMeta<Date>) {
  return {
    ...post,
    date: post.date.toISOString(),
  };
}

export type Course = Tagged & {
  name: string;
  number: string;
  slug: string;
  date: string;
  description: string;
};

export type Education = {
  name: string;
  degree: string | null;
  startDate: string;
  endDate: string;
  slug: string;
  courses: Course[];
  gpa: number;
};

export type SkillGroup = {
  name: string;
  skills: {
    level: number;
    tag: Tag;
  }[];
};
