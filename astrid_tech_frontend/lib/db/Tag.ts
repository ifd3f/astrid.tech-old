import {
  getContrastingTextColor,
  getHSLString,
  getPersistentColor,
} from "../util";
import {
  Entity,
  PrimaryGeneratedColumn,
  Column,
  Unique,
  Connection,
} from "typeorm";

@Entity()
@Unique(["shortName"])
export class Tag {
  @PrimaryGeneratedColumn()
  id?: number;

  @Column({ nullable: false })
  shortName!: string;

  @Column({ nullable: false })
  title!: string;

  @Column({ nullable: false })
  backgroundColor!: string;

  @Column({ nullable: false })
  color!: string;
}

export function fillTagValues({
  shortName,
  title,
  backgroundColor,
  color,
}: {
  shortName: string;
  title?: string;
  backgroundColor?: string;
  color?: string;
}): Tag {
  const title_ = title ?? shortName;
  const backgroundColor_ =
    backgroundColor ?? getHSLString(getPersistentColor(shortName));
  const color_ = color ?? getContrastingTextColor(backgroundColor_);

  return {
    shortName,
    title: title_,
    backgroundColor: backgroundColor_,
    color: color_,
  };
}

export async function getOrCreateTag(conn: Connection, shortName: string) {
  const repo = conn.getRepository(Tag);
  const result = repo.findOne({ shortName });
  if (result) {
    return result;
  }

  const tag = repo.create(fillTagValues({ shortName }));
  await repo.save(tag);
  return tag;
}
