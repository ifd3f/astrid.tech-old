import {
  Entity,
  PrimaryGeneratedColumn,
  Column,
  OneToOne,
  JoinColumn,
} from "typeorm";
import { Page } from "./Page";
import { TimeSlug } from "./TimeSlug";

@Entity()
export class Article {
  @PrimaryGeneratedColumn()
  id!: number;

  @OneToOne((type) => Page, { onDelete: "CASCADE" })
  @JoinColumn()
  page!: Page;

  @OneToOne((type) => TimeSlug, { onDelete: "CASCADE" })
  @JoinColumn()
  slug!: TimeSlug;
}
