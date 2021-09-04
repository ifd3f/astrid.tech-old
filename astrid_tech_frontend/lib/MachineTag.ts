const REGEX = /([a-z0-9-]+):([a-z0-9-]+)(?:=([a-z0-9-]+))?/i;

export class MachineTag {
  public namespace: string = '';
  public predicate: string = '';
  public value: string | null = null;

  constructor(tag: string);
  constructor(namespace: string, predicate?: string, value?: string) {
    if (predicate) {
      this.namespace = namespace;
      this.predicate = predicate;
      this.value = value ?? null;
    } else {
      this.tag = namespace;
    }
  }

  get tag() {
    if (this.value) {
      return `${this.namespace}:${this.predicate}=${this.value}`;
    }
    return `${this.namespace}:${this.predicate}`;
  }

  set tag(tag: string) {
    const match = tag.match(REGEX);
    if (match) {
      [, this.namespace, this.predicate, this.value] = match;
      this.value = this.value ?? null;
    } else {
      throw new Error(`Invalid machine tag ${tag}`);
    }
  }
}

export function parseMachineTagOrNull(tag: string): MachineTag | null {
  try {
    return new MachineTag(tag);
  } catch (e) {
    return null;
  }
}
