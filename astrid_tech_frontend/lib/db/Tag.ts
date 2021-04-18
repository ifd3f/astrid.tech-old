import {
  Entity,
  PrimaryGeneratedColumn,
  Column,
  JoinTable,
  OneToMany,
  Unique,
} from "typeorm";

@Entity()
@Unique(["name"])
export class Tag {
  @PrimaryGeneratedColumn()
  id!: number;

  @Column({ nullable: false })
  slug!: string;

  @Column({ nullable: false })
  name!: string;

  @Column({ nullable: false })
  backgroundColor!: string;

  @Column({ nullable: false })
  color!: string;
}
