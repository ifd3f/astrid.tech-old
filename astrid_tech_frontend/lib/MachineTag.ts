export class MachineTag {
  public namespace: string = "";
  public predicate: string = "";
  public value: string | null = null;

  get tag() {
    if (this.value) {
      return `${this.namespace}:${this.predicate}=${this.value}`;
    }
    return `${this.namespace}:${this.predicate}`;
  }

  set tag(tag: string) {
    const match = tag.match(/([a-z0-9-]+):([a-z0-9-]+)(?:=([a-z0-9-]+))?/i);
    if (match) {
      [, this.namespace, this.predicate, this.value] = match;
      this.value = this.value ? this.value : null;
    } else {
      throw new Error(`Invalid machine tag ${tag}`);
    }
  }
}
