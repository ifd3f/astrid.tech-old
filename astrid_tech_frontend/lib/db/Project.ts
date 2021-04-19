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
export class Project {
  @PrimaryGeneratedColumn()
  id?: number;

  @Column()
  shortName!: string;

  @Column({ nullable: true })
  status?: string;

  @Column({ nullable: false })
  startDate!: Date;

  @Column({ nullable: true })
  endDate?: Date;

  @Column("simple-array")
  source!: string[];

  @Column("simple-array")
  url!: string[];

  @OneToOne((type) => Page, { onDelete: "CASCADE" })
  @JoinColumn()
  page!: Page;
}
