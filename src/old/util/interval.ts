export class Interval {
  constructor(public start: Date, public end: Date) {}
}

export class MultiInterval {
  constructor(public intervals: Interval[]) {}
}
