import path from "path";

export function getResource(subpath: string) {
  return path.join(__dirname, "../test-resources", subpath);
}
