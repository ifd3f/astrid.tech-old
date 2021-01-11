import assert from "assert";
import { MachineTag } from "./MachineTag";

const cases = [
  ["foo", "bar", "spam"],
  ["school", "cal-poly", "csc-400"],
  ["A32", "b-9-82", "faB-s789"],
];

describe("MachineTag", function () {
  describe("constructor", function () {
    cases.forEach(([n, p, v]) => {
      const tag = `${n}:${p}=${v}`;
      it(`should parse NPV (value=${tag})`, function () {
        const parsed = new MachineTag(tag);
        assert.strictEqual(parsed.namespace, n);
        assert.strictEqual(parsed.predicate, p);
        assert.strictEqual(parsed.value, v);
      });
    });

    it("should parse NP", function () {
      const tag = new MachineTag("foo:bar");
      assert.strictEqual(tag.namespace, "foo");
      assert.strictEqual(tag.predicate, "bar");
      assert.strictEqual(tag.value, null);
    });
  });
});
