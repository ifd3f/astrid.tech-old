import {
  Entity,
  PrimaryGeneratedColumn,
  Column,
  OneToOne,
  JoinColumn,
} from "typeorm";
import { TimeSlug } from "./TimeSlug";

@Entity()
export class Note {
  @PrimaryGeneratedColumn()
  id!: number;

  @Column({ nullable: false })
  contentMarkdown!: String;

  @OneToOne((type) => TimeSlug)
  @JoinColumn()
  slug!: TimeSlug;

  @Column({ nullable: false })
  date!: Date;
}
