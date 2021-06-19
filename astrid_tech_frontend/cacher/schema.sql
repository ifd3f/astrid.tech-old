CREATE TABLE tag (
  slug TEXT PRIMARY KEY NOT NULL,
  name TEXT,
  color INTEGER,
  background_color INTEGER
);
CREATE TABLE project (
  id INTEGER PRIMARY KEY AUTOINCREMENT NOT NULL,
  asset_root TEXT NOT NULL,
  title TEXT NOT NULL,
  description TEXT,
  slug TEXT NOT NULL,
  start_date DATETIME NOT NULL,
  end_date DATETIME,
  url TEXT,
  status TEXT,
  source_urls TEXT,
  thumbnail_path TEXT,
  content TEXT NOT NULL
);
CREATE TABLE blog_post (
  id INTEGER PRIMARY KEY AUTOINCREMENT NOT NULL,
  asset_root TEXT NOT NULL,
  title TEXT NOT NULL,
  description TEXT,
  thumbnail TEXT,
  slug TEXT NOT NULL,
  date DATETIME NOT NULL,
  year INTEGER NOT NULL,
  month INTEGER NOT NULL,
  day INTEGER NOT NULL,
  ordinal INTEGER NOT NULL,
  content TEXT NOT NULL,
  UNIQUE (year, month, day, ordinal)
);
CREATE INDEX idx_blog ON blog_post(year, month, day, ordinal);
CREATE TABLE project_tag (
  id INTEGER PRIMARY KEY AUTOINCREMENT NOT NULL,
  fk_project INTEGER NOT NULL,
  tag TEXT NOT NULL,
  FOREIGN KEY(fk_project) REFERENCES project(id),
  UNIQUE(fk_project, tag)
);
CREATE INDEX idx_project_to_tag ON project_tag(tag, fk_project);
CREATE INDEX idx_tag_to_project ON project_tag(fk_project, tag);
CREATE TABLE blog_tag (
  id INTEGER PRIMARY KEY AUTOINCREMENT NOT NULL,
  fk_blog INTEGER NOT NULL,
  tag TEXT NOT NULL,
  FOREIGN KEY(fk_blog) REFERENCES blog_post(id),
  UNIQUE(fk_blog, tag)
);
CREATE INDEX idx_blog_to_tag ON blog_tag(tag, fk_blog);
CREATE INDEX idx_tag_to_blog ON blog_tag(fk_blog, tag);