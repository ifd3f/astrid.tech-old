import assert from "assert";
import { MachineTag } from "./MachineTag";

describe("MachineTag", function () {
  describe("constructor", function () {
    it("should parse NPV", function () {
      const tag = new MachineTag("foo:bar=spam");
      assert.strictEqual(tag.namespace, "foo");
      assert.strictEqual(tag.predicate, "bar");
      assert.strictEqual(tag.value, "spam");
    });

    it("should parse NP", function () {
      const tag = new MachineTag("foo:bar");
      assert.strictEqual(tag.namespace, "foo");
      assert.strictEqual(tag.predicate, "bar");
      assert.strictEqual(tag.value, null);
    });
  });
});
