import { Entity, PrimaryGeneratedColumn, Column, JoinTable } from "typeorm";
import { Page } from "./Page";
import { TimeSlug } from "./TimeSlug";

@Entity()
export class Project {
  @PrimaryGeneratedColumn()
  id!: number;

  @Column({ nullable: false, unique: true })
  page!: Page;

  @Column({ nullable: false })
  slug!: TimeSlug;

  @Column({ nullable: false })
  startDate!: Date;

  @Column()
  endDate!: Date;
}
