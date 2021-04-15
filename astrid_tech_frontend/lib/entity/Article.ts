import { Entity, PrimaryGeneratedColumn, Column, JoinTable } from "typeorm";
import { Page } from "./Page";
import { TimeSlug } from "./TimeSlug";

@Entity()
export class Article {
  @PrimaryGeneratedColumn()
  id!: number;

  @Column({ nullable: false, unique: true })
  page!: Page;

  @Column({ nullable: false })
  slug!: TimeSlug;

  @Column({ nullable: false })
  date!: Date;
}
