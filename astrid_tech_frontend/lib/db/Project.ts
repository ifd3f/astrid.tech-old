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
  id!: number;

  @OneToOne((type) => Page)
  @JoinColumn()
  page!: Page;

  @Column({ nullable: false })
  startDate!: Date;

  @Column()
  endDate!: Date;
}
