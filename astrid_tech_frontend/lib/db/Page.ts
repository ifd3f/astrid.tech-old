import {
  Entity,
  PrimaryGeneratedColumn,
  Column,
  ManyToMany,
  JoinColumn,
} from "typeorm";
import { Tag } from "./Tag";

@Entity("Page")
export class Page {
  @PrimaryGeneratedColumn()
  id!: number;

  @Column()
  date!: Date;

  @Column()
  pathname!: string;

  @Column()
  assetRoot!: string;

  @Column()
  title!: string;

  @Column()
  thumbnail!: string;

  @Column()
  contentMarkdown!: string;

  @Column()
  objectType!: string;

  @ManyToMany((type) => Tag)
  @JoinColumn()
  tags!: Tag;
}
