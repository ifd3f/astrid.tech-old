import { Entity, PrimaryGeneratedColumn, Column } from "typeorm";

@Entity()
export class TimeSlug {
  @PrimaryGeneratedColumn()
  id!: number;

  @Column({ nullable: false })
  date!: Date;

  @Column({ nullable: false })
  year!: number;

  @Column({ nullable: false })
  month!: number;

  @Column({ nullable: false })
  day!: number;

  @Column({ nullable: false })
  ordinal!: number;

  @Column({ nullable: false })
  shortName!: string;

  @Column({ nullable: true })
  objectType!: string;
}

export function timeSlugToString(path: TimeSlug) {
  return `/${path.year}/${path.month}/${path.day}/${path.ordinal}/${path.shortName}`;
}
