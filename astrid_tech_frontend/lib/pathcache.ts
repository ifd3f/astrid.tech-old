import AsyncRedis from "async-redis";
import { GetStaticPaths, GetStaticPathsContext } from "next";

type Wrapped = GetStaticPaths & { getStringPaths?: () => Promise<string[]> };

export function wrappedStaticPaths(
  filename: string,
  getStaticPaths: GetStaticPaths,
  formatter?: (fmt: any) => string
): Wrapped {
  const f = (process.env.NODE_ENV == "production" ||
  process.env.FORCE_CACHE == "1"
    ? async (ctx: GetStaticPathsContext) => {
        const key = "cachedpath-" + filename;

        const redis = new AsyncRedis();
        if (await redis.exists(key)) {
          const result = ((await redis.get(key)) as unknown) as string;
          console.log(result);
          return JSON.parse(result);
        }

        const result = await getStaticPaths(ctx);
        await redis.set(key, JSON.stringify(result));

        return result;
      }
    : getStaticPaths) as Wrapped;

  f.getStringPaths = async () => {
    return (await f({})).paths.map((param: any) =>
      typeof param == "string" ? param : formatter!(param.params)
    );
  };

  return f;
}
