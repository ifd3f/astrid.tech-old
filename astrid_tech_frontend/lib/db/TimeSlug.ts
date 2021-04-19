import { Entity, PrimaryGeneratedColumn, Column } from "typeorm";

@Entity()
export class TimeSlug {
  @PrimaryGeneratedColumn()
  id?: number;

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

  @Column({ nullable: false })
  objectType!: string;

  static create({
    date,
    ordinal,
    objectType,
    shortName,
  }: {
    date: Date;
    ordinal: number;
    objectType: string;
    shortName: string;
  }): TimeSlug {
    return {
      date,
      objectType,
      year: date.getUTCFullYear(),
      month: date.getUTCMonth() + 1,
      day: date.getUTCDate(),
      ordinal,
      shortName,
    };
  }
}

export function timeSlugToString(path: TimeSlug) {
  return `/${path.year}/${path.month}/${path.day}/${path.ordinal}/${path.shortName}`;
}
