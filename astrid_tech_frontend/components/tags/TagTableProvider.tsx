import { createContext, FC, ReactNode, useContext } from "react";
import {
  getContrastingTextColor,
  getHSLString,
  getPersistentColor,
  RichColorTheme,
} from "../../lib/util";
import { Tag } from "../../types/types";

export class TagTable {
  private readonly cache: Map<string, Tag>;
  constructor(data: Tag[]) {
    this.cache = new Map(data.map((t) => [t.slug, t]));
  }

  get(tag: Tag | string): Tag {
    if (typeof tag != "string") {
      return tag;
    }

    const existing = this.cache.get(tag);
    if (existing) return existing;

    const backgroundColor = getHSLString(
      getPersistentColor(tag, RichColorTheme)
    );
    const color = getContrastingTextColor(backgroundColor);
    const result: Tag = {
      name: tag,
      color,
      backgroundColor,
      slug: tag,
    };
    this.cache.set(tag, result);
    return result;
  }
}

const Context = createContext<TagTable>({} as TagTable);

export type TagTableProviderProps = { tags: Tag[]; children: ReactNode };

export const TagTableProvider: FC<TagTableProviderProps> = ({
  tags,
  children,
}) => {
  return (
    <Context.Provider value={new TagTable(tags)}>{children}</Context.Provider>
  );
};

export const useTagTable = () => useContext(Context);
