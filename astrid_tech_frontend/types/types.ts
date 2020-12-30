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

export type Project = Tagged & {
  __typename: "Project";
  title: string;
  status: null | "early" | "wip" | "complete" | "scrapped";
  featured: boolean;
  startDate: string;
  endDate: string | null;
  slug: string;
  url: string;
  source: string[];
  thumbnail: File;
  content: string;
  description?: string;
  childProjectTag: { childTag: Tag };
  highlights?: string[];
  keywords?: string[];
  internal: {
    content: string;
    description: string;
  };
};

export type BlogPost<DateType = Date> = Tagged & {
  title: string;
  description: string;
  date: DateType;
  slug: string;
  thumbnail: string | null;
  content: string;
};

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
