import rimraf from "rimraf";
import path from "path";
import { createConnection } from "../lib/db/index";

const contentDir = path.join(process.cwd(), "content");

async function main() {
  const dataDir = path.join(__dirname, "../data");
  rimraf(dataDir, console.error);

  const connection = await createConnection();

  await connection.close();
}

main().catch((e) => {
  throw e;
});
