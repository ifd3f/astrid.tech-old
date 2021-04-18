import path from "path";
import { createConnection } from "../lib/db";
import { clearCaches } from "./util";

const contentDir = path.join(process.cwd(), "content");

async function main() {
  clearCaches();

  const connection = await createConnection();

  await connection.close();
}

main().catch((e) => {
  throw e;
});
