import assert from "assert";
import { MachineTag } from "./MachineTag";

describe("MachineTag", function () {
  describe("set tag", function () {
    it("should parse NPV", function () {
      const tag = new MachineTag();
      tag.tag = "foo:bar=spam";
      assert.strictEqual(tag.namespace, "foo");
      assert.strictEqual(tag.predicate, "bar");
      assert.strictEqual(tag.value, "spam");
    });

    it("should parse NP", function () {
      const tag = new MachineTag();
      tag.tag = "foo:bar";
      assert.strictEqual(tag.namespace, "foo");
      assert.strictEqual(tag.predicate, "bar");
      assert.strictEqual(tag.value, null);
    });
  });
});
