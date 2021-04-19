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

type TagPrototype = {
  shortName: string;
  title?: string;
  backgroundColor?: string;
  color?: string;
};

export function fillTagValues({
  shortName,
  title,
  backgroundColor,
  color,
}: TagPrototype): Tag {
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

export async function getOrCreateTag(
  conn: Connection,
  prototype: TagPrototype
) {
  const repo = conn.getRepository(Tag);
  const result = await repo.findOne(prototype);
  if (result) {
    return result;
  }

  return await repo.save(fillTagValues(prototype));
}
