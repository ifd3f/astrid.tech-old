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

  get(slug: string): Tag {
    const existing = this.cache.get(slug);
    if (existing) return existing;

    const backgroundColor = getHSLString(
      getPersistentColor(slug, RichColorTheme)
    );
    const color = getContrastingTextColor(backgroundColor);
    const tag: Tag = {
      name: slug,
      color,
      backgroundColor,
      slug,
    };
    this.cache.set(slug, tag);
    return tag;
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
