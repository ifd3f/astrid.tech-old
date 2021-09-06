import classNames from "classnames";
import { resolveAssetURL } from "lib/cache/assets";
import Link from "next/link";
import { CSSProperties, FC, useRef } from "react";
import {
  Badge,
  CardBody,
  CardText,
  CardTitle,
  UncontrolledTooltip,
} from "reactstrap";
import { getPersistentColor, PastelTheme } from "../../lib/util";
import { Project, ProjectMeta } from "../../types/types";
import { TagList } from "../tags/tag";
import styles from "./project.module.scss";

type StatusBadgeProps = {
  status: Project["status"];
};

export const StatusBadge: FC<StatusBadgeProps> = ({ status }) => {
  const badgeId = useRef(null);

  let title: string, tooltip: string, color: string;
  switch (status) {
    case "wip":
      title = "WIP";
      tooltip = "I am currently working on this.";
      color = "info";
      break;
    case "complete":
      title = "Complete";
      tooltip = "This project is complete!";
      color = "success";
      break;
    case "scrapped":
      title = "Scrapped";
      tooltip = "I decided it wasn't worth pursuing anymore.";
      color = "danger";
      break;
    default:
      return null;
  }
  return (
    <Badge ref={badgeId} color={color}>
      {title}
      <UncontrolledTooltip placement="top" target={badgeId}>
        {tooltip}
      </UncontrolledTooltip>
    </Badge>
  );
};

type ProjectCardProps = {
  project: ProjectMeta<Date>;
  hovered?: boolean;
};

export const ProjectCard: FC<ProjectCardProps> = ({
  project,
  hovered = false,
}) => {
  const [h, s, v] = getPersistentColor(project.slug, PastelTheme);
  const color = `hsl(${h}, ${s}%, ${v}%)`;

  let style: CSSProperties = { backgroundColor: color };
  if (project.thumbnail) {
    style = {
      ...style,
      backgroundImage:
        `linear-gradient(to bottom, hsla(${h}, 80%, 80%, 1.0), hsla(${h}, 80%, 80%, 0.9), hsla(${h}, 80%, 80%, 0.6), rgba(0.6, 0.6, 0.6, 0.3)), ` +
        `url(${resolveAssetURL(project.assetRoot, project.thumbnail)})`,
      backgroundRepeat: "no-repeat",
      backgroundPosition: "center",
      backgroundSize: "cover",
    };
  }

  return (
    <CardBody
      style={style}
      className={classNames(
        hovered ? styles.hoveredProjectCard : null,
        styles.projectCard
      )}
    >
      <div className={classNames(styles.upper, "h-entry")}>
        <Link href={`/projects/${project.slug}`}>
          <a style={{ color: "black" }}>
            <CardTitle>
              <h3 className={classNames(styles.title, "p-name")}>
                {project.title}
              </h3>
            </CardTitle>
            <CardText>
              <p className={classNames(styles.subtitle, "p-summary")}>
                {project.description}
              </p>
            </CardText>
          </a>
        </Link>
      </div>
      <div className={classNames(styles.lower, "e-content")}>
        <TagList tags={project.tags} limit={5} className={styles.tags} link />
      </div>
    </CardBody>
  );
};
