CREATE TABLE tag (
  code TEXT PRIMARY KEY NOT NULL,
  name TEXT,
  color INTEGER,
  background_color INTEGER
);
CREATE TABLE project (
  id INTEGER PRIMARY KEY AUTOINCREMENT,
  title TEXT NOT NULL,
  slug TEXT NOT NULL,
  start_date DATETIME NOT NULL,
  end_date DATETIME,
  url TEXT,
  status TEXT,
  source_url TEXT,
  thumbnail_path TEXT,
  content TEXT NOT NULL
);
CREATE TABLE project_to_tag (
  id INTEGER PRIMARY KEY AUTOINCREMENT,
  fk_project INTEGER NOT NULL,
  fk_tag TEXT NOT NULL,
  FOREIGN KEY(fk_project) REFERENCES project(id),
  FOREIGN KEY(fk_tag) REFERENCES tag(slug),
  UNIQUE(fk_project, fk_tag)
);
CREATE TABLE blog (
  id INTEGER PRIMARY KEY AUTOINCREMENT,
  title TEXT NOT NULL,
  slug TEXT NOT NULL,
  date DATETIME NOT NULL,
  content TEXT NOT NULL
);
CREATE TABLE blog_to_tag (
  id INTEGER PRIMARY KEY AUTOINCREMENT,
  fk_blog INTEGER NOT NULL,
  fk_tag TEXT NOT NULL,
  FOREIGN KEY(fk_blog) REFERENCES blog(id),
  FOREIGN KEY(fk_tag) REFERENCES tag(slug),
  UNIQUE(fk_blog, fk_tag)
);