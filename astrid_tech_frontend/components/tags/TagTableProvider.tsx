import { useAPI } from "components/api/APIProvider";
import { AstridTechAPI } from "lib/astrid-tech-api";
import {
  createContext,
  useEffect,
  FC,
  ReactNode,
  useContext,
  useState,
} from "react";
import { parseMachineTagOrNull } from "../../lib/MachineTag";
import {
  getContrastingTextColor,
  getHSLString,
  getPersistentColor,
  RichColorTheme,
} from "../../lib/util";
import { Tag } from "../../types/types";

export interface TagTable {
  get(id: string): Tag;
}

class EmptyTagTable implements TagTable {
  get(id: string): Tag {
    if (typeof id != "string") {
      return id;
    }
    return {
      name: id,
      backgroundColor: "#333333",
      color: "#FFFFFF",
      id,
    };
  }
}

class FilledTagTable {
  private readonly cache: Map<string, Tag>;

  constructor(data: Tag[]) {
    this.cache = new Map(data.map((t) => [t.id, t]));
  }

  get(id: string): Tag {
    const existing = this.cache.get(id);
    if (existing) return existing;

    const backgroundColor = getHSLString(
      getPersistentColor(id, RichColorTheme)
    );
    const color = getContrastingTextColor(backgroundColor);

    var name = id;

    const machineTag = parseMachineTagOrNull(id);
    if (machineTag) {
      switch (machineTag.namespace) {
        case "school":
          switch (machineTag.predicate) {
            case "cal-poly":
              name = machineTag.value!.replace("-", " ").toUpperCase();
          }
      }
    }

    const result: Tag = {
      name,
      color,
      backgroundColor,
      id,
    };
    this.cache.set(id, result);
    return result;
  }
}

const Context = createContext<{ table: TagTable }>({
  table: new EmptyTagTable(),
});

export type TagTableProviderProps = { children: ReactNode };

export type TagTableContext = { table: TagTable };

export const TagTableProvider: FC<TagTableProviderProps> = ({ children }) => {
  const { api } = useAPI();
  const [table, setTable] = useState<TagTable>(new EmptyTagTable());
  useEffect(() => {
    api.getTags().then((tags) => setTable(new FilledTagTable(tags)));
  }, []);
  return <Context.Provider value={{ table }}>{children}</Context.Provider>;
};

export const useTagTable = () => useContext(Context);
