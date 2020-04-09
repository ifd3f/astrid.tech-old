export class Interval {
  constructor(public start: Date, end: Date) {}
}

export class MultiInterval {
  constructor(public intervals: Interval[]) {}
}
