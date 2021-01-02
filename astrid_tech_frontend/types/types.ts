export type Tagged<TypeID = undefined> = {
  type?: TypeID;
  tags: string[];
};

export type Tag = {
  name: string;
  color: string;
  backgroundColor: string;
  slug: string;
  count?: number;
};

export type WorkExperience = Tagged<"w"> & {
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

export type ProjectMeta<DateType = Date> = Tagged<"p"> & {
  id?: number;
  excerpt?: string;
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

export function convertProjectToObjectDate<T>(
  project: ProjectMeta<string> & T
): ProjectMeta<Date> & T {
  return {
    ...project,
    startDate: new Date(project.startDate),
    endDate: project.endDate ? new Date(project.endDate) : null,
  };
}

export function convertProjectToStringDate<T>(
  project: ProjectMeta<Date> & T
): ProjectMeta<string> & T {
  return {
    ...project,
    startDate: project.startDate.toISOString(),
    endDate: project.endDate ? project.endDate.toISOString() : null,
  };
}

export type BlogPostMeta<DateType = Date> = Tagged<"b"> & {
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

export function convertBlogPostToObjectDate<T>(
  post: BlogPostMeta<string> & T
): BlogPostMeta<Date> & T {
  return {
    ...post,
    date: new Date(post.date),
  };
}

export function convertBlogPostToStringDate<T>(
  post: BlogPostMeta<Date> & T
): BlogPostMeta<string> & T {
  return {
    ...post,
    date: post.date.toISOString(),
  };
}

export type SiteObject = ProjectMeta<string> | BlogPostMeta<string>;

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
