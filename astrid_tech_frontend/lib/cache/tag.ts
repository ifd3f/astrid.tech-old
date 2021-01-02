import { BlogPost, Project } from "../../types/types";
import { rowToBlogPost } from "./blog";
import { rowToProject } from "./project";
import { getConnection } from "./util";

export function getTags(): string[] {
  const db = getConnection();
  const results = db
    .prepare(
      `SELECT DISTINCT tag FROM project_tag
      UNION
      SELECT DISTINCT tag from blog_tag`
    )
    .all();

  return results.map(({ tag }) => tag as string);
}

export function getTagged(tag: string): (BlogPost<string> | Project<string>)[] {
  const db = getConnection();
  const results = db
    .prepare(
      `SELECT 'p' AS type, project.id as id, project.assetRoot as string, project.title AS title, NULL as date, start_date, end_date, content
      FROM project_tag
      LEFT JOIN project
        ON fk_project = project.id
      WHERE tag = @tag
      UNION 
        SELECT 'b' AS type, blog_post.id as id, blog_post.assetRoot as string, blog_post.title, date, NULL, NULL, content
        FROM blog_tag
        LEFT JOIN blog_post
          ON fk_blog = blog_post.id
        WHERE tag = @tag`
    )
    .all({ tag });

  return results.map((object) => {
    switch (object.type) {
      case "b":
        return rowToBlogPost(object, []);
      case "p":
        return rowToProject(object, []);
    }
    throw new Error();
  });
}
