import { Database } from 'better-sqlite3';
import { Project, ProjectMeta } from "../types";

export function rowToProjectMeta(
  row: any,
  tags: string[]
): ProjectMeta<string> {
  return {
    ...row,
    source: JSON.parse(row.source!!) as string[],
    thumbnail: row.thumbnail as string,
    tags,
  };
}

export function rowToProject(row: any, tags: string[]): Project<string> {
  return {
    ...rowToProjectMeta(row, tags),
    content: row.content as string,
  };
}

export function getProjectSlugs(db: Database): string[] {
  return db
    .prepare("SELECT slug FROM project")
    .all()
    .map(({ slug }) => slug);
}

export function getProject(db: Database, slug: string): Project<string> {
  const project = db
    .prepare(
      `SELECT 
        id, 
        slug,
        asset_root as assetRoot, 
        title, 
        status,
        description, 
        start_date as startDate, 
        end_date as endDate,
        url,
        source_urls as source,
        thumbnail_path as thumbnail,
        content
      FROM project
      WHERE @slug = slug`
    )
    .get({ slug });

  const tags = db
    .prepare(`SELECT tag FROM project_tag WHERE fk_project = @id`)
    .all({ id: project.id })
    .map((r) => r.tag);

  return rowToProject(project, tags);
}

export type ProjectLink = {
  slug: string;
  title: string;
};

export function getSimilarProjects(db: Database, id: number): ProjectLink[] {
  const projects = db
    .prepare(
      `SELECT slug, title, SUM(t3.weight) as score FROM project
      LEFT JOIN project_tag AS t2
        ON project.id = t2.fk_project
      LEFT JOIN (SELECT 1.0 / COUNT(fk_project) AS weight, tag FROM project_tag GROUP BY tag) AS t3
        ON t3.tag = t2.tag
      WHERE project.id != @id AND t2.tag IN (SELECT tag FROM project_tag WHERE project_tag.fk_project = @id)
      GROUP BY slug
      ORDER BY score DESC
      LIMIT 5`
    )
    .all({ id });
  return projects.map((project) => ({
    slug: project.slug as string,
    title: project.title as string,
  }));
}

export function getProjects(db: Database): Project<string>[] {
  const projects = db
    .prepare(
      `SELECT 
        id,
        slug,
        asset_root as assetRoot, 
        title, 
        status,
        description, 
        start_date as startDate, 
        end_date as endDate,
        url,
        source_urls as source,
        thumbnail_path as thumbnail,
        content
      FROM project 
      ORDER BY 
        end_date DESC NULLS FIRST, 
        slug ASC
      `
    )
    .all();

  const tags = db.prepare(`SELECT fk_project, tag FROM project_tag`).all();

  const idToProject = new Map<number, Project<string>>(
    projects.map((row) => [row.id as number, rowToProject(row, [])])
  );

  for (const { fk_project, tag } of tags) {
    idToProject.get(fk_project)!!.tags.push(tag as string);
  }

  return [...idToProject.values()];
}
