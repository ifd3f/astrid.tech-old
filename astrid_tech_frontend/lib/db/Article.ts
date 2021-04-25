import { Entity, PrimaryGeneratedColumn, OneToOne, JoinColumn } from "typeorm";
import { Page } from "./Page";
import { TimeSlug } from "./TimeSlug";

@Entity()
export class Article {
  @PrimaryGeneratedColumn()
  id!: number;

  @OneToOne((type) => TimeSlug, { onDelete: "CASCADE" })
  @JoinColumn()
  slug!: TimeSlug;

  @OneToOne((type) => Page, { onDelete: "CASCADE" })
  @JoinColumn()
  page!: Page;
}
