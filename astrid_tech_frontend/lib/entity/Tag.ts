import { Entity, PrimaryGeneratedColumn, Column, JoinTable } from "typeorm";

@Entity()
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
