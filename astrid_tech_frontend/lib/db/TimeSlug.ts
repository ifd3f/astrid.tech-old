import { Entity, PrimaryGeneratedColumn, Column, JoinTable } from "typeorm";

@Entity()
export class TimeSlug {
  @PrimaryGeneratedColumn()
  id!: number;

  @Column({ nullable: false })
  date!: Date;

  @Column()
  year!: number;

  @Column()
  month!: number;

  @Column()
  day!: number;

  @Column()
  ordinal!: number;

  @Column()
  shortName!: string;

  @Column()
  objectType!: string;
}
