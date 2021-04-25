import { Entity, PrimaryGeneratedColumn, OneToOne, JoinColumn } from "typeorm";
import { Page } from "./Page";
import { TimeSlug } from "./TimeSlug";

@Entity()
export class Note {
  @PrimaryGeneratedColumn()
  id!: number;

  @OneToOne((type) => TimeSlug)
  @JoinColumn()
  slug!: TimeSlug;

  @OneToOne((type) => Page, { onDelete: "CASCADE" })
  @JoinColumn()
  page!: Page;
}
