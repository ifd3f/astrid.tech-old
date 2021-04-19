import {
  Entity,
  PrimaryGeneratedColumn,
  Column,
  ManyToMany,
  JoinTable,
} from "typeorm";
import { Tag } from "./Tag";

@Entity("Page")
export class Page {
  @PrimaryGeneratedColumn()
  id?: number;

  @Column()
  date!: Date;

  @Column()
  pathname!: string;

  @Column()
  assetRoot!: string;

  @Column()
  title!: string;

  @Column({ nullable: true })
  thumbnail!: string;

  @Column()
  contentMarkdown!: string;

  @Column()
  objectType!: string;

  @ManyToMany((type) => Tag)
  @JoinTable()
  tags!: Tag[];
}
