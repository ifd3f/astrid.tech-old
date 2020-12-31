import { Project, ProjectMeta, ProjectStatus } from "../../types/types";
import { getConnection } from "./util";

export function getProjectSlugs(): string[] {
  const db = getConnection();
  return db
    .prepare("SELECT slug FROM project")
    .all()
    .map(({ slug }) => slug);
}

export function getProject(slug: string): Project<string> {
  const db = getConnection();
  const {
    id,
    assetRoot,
    title,
    status,
    description,
    startDate,
    endDate,
    url,
    source,
    thumbnail,
    content,
  } = db
    .prepare(
      `SELECT 
        id, 
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
    .all({ id });

  return {
    assetRoot,
    id: id,
    title: title as string,
    status: status as ProjectStatus,
    description: description as string,
    slug: slug as string,
    startDate: startDate as string,
    endDate: endDate as string | null,
    url: url as string,
    source: JSON.parse(source) as string[],
    content: content as string,
    thumbnail: thumbnail as string,
    tags: tags.map(({ tag }) => tag),
  };
}

export function getSimilarProjects(id: number) {
  `select slug, sum(t3.score) from project
  left join project_tag as t2
	on project.id = t2.fk_project
  left join (select 1.0 / count(fk_project) as score, tag from project_tag group by tag) as t3
	on t3.tag = t2.tag
  where project.id != 1 and t2.tag in (select tag from project_tag where project_tag.fk_project = 1)
group by slug`; // TODO do something with this
}

export function getProjectMetas(): ProjectMeta<string>[] {
  const db = getConnection();
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
        thumbnail_path as thumbnail
      FROM project
      ORDER BY 
        end_date DESC NULLS FIRST, 
        slug ASC
      `
    )
    .all();

  const tags = db.prepare(`SELECT fk_project, tag FROM project_tag`).all();

  const idToProject = new Map<number, ProjectMeta<string>>(
    projects.map(
      ({
        id,
        slug,
        assetRoot,
        title,
        status,
        description,
        startDate,
        endDate,
        url,
        source,
        thumbnail,
      }) => [
        id as number,
        {
          title: title as string,
          assetRoot: assetRoot as string,
          status: status as ProjectStatus,
          description: description as string,
          slug: slug as string,
          startDate: startDate as string,
          endDate: endDate as string | null,
          url: url as string,
          source: JSON.parse(source) as string[],
          thumbnail: thumbnail as string,
          tags: [],
        },
      ]
    )
  );

  for (const { fk_project, tag } of tags) {
    idToProject.get(fk_project)!!.tags.push(tag as string);
  }

  return [...idToProject.values()];
}
