import { GetStaticPaths } from 'next';

type Wrapped = GetStaticPaths & { getStringPaths?: () => Promise<string[]> };

export function wrappedStaticPaths(
  filename: string,
  getStaticPaths: GetStaticPaths,
  formatter?: (fmt: any) => string
): Wrapped {
  (getStaticPaths as Wrapped).getStringPaths = async () => {
    return (await getStaticPaths({})).paths.map((param: any) =>
      typeof param == 'string' ? param : formatter!(param.params)
    );
  };

  return getStaticPaths;
}
