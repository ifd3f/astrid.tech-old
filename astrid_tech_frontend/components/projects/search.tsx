import Fuse from "fuse.js";
import React, { createContext, FC, ReactNode, useState } from "react";
import { Project, ProjectMeta } from "../../types/types";

function countTagUsages(projects: ProjectMeta[]) {
  const count = new Map<string, number>();
  for (const project of projects) {
    for (const tag of project.tags) {
      count.set(tag, 1 + (count.get(tag) ?? 0));
    }
  }
  return count;
}

export type SearchContext = {
  tagUsageCounts: Map<string, number>;
  selectableTags: string[];
  projects: ProjectMeta[];

  displayedProjects: ProjectMeta[];

  isSearching: boolean;
  searchString: string;
  setSearchString: (searchString: string) => void;

  filterTags: string[];
  addFilterTag: (slug: string) => void;
  removeFilterTag: (slug: string) => void;
  clearFilterTags: () => void;

  shouldFilterAny: boolean;
  setShouldFilterAny: (shouldFilterAny: boolean) => void;
};

export const SearchContext = createContext<SearchContext>({} as any);

export type FiltererArgs = {
  children: ReactNode;
  projects: ProjectMeta[];
  fuse: Fuse<ProjectMeta>;
};

export const Filterer: FC<FiltererArgs> = ({ children, projects, fuse }) => {
  const [searchString, _setSearchString] = useState("");
  const [filterTags, setFilterTags] = useState<string[]>([]);
  const [shouldFilterAny, _setShouldFilterAnyTags] = useState<boolean>(false);

  const isSearching = searchString.length != 0;

  const setSearchString = (searchString: string) => {
    _setSearchString(searchString);
  };

  const setShouldFilterAny = (shouldFilterAny: boolean) => {
    _setShouldFilterAnyTags(shouldFilterAny);
  };

  const addFilterTag = (slug: string) => {
    setFilterTags([...filterTags, slug]);
  };

  const removeFilterTag = (slug: string) => {
    setFilterTags(filterTags.filter((tag) => tag != slug));
  };

  const clearFilterTags = () => {
    setFilterTags([]);
  };

  const filterTagsSet = new Set(filterTags);
  var displayedProjects =
    searchString == ""
      ? projects
      : (fuse.search(searchString).map((result: any) => {
          return result.item;
        }) as Project[]);

  if (filterTags.length > 0) {
    displayedProjects = displayedProjects.filter((project) => {
      const filteredCount = project.tags.filter((tag) => filterTagsSet.has(tag))
        .length;

      return shouldFilterAny
        ? filteredCount > 0
        : filteredCount == filterTags.length;
    });
  }

  const tagUsageCounts = countTagUsages(displayedProjects);
  const orderedTags = displayedProjects
    .flatMap((project) => project.tags)
    .sort((a, b) => tagUsageCounts.get(b)! - tagUsageCounts.get(a)!);
  const slugToTag = new Map(orderedTags.map((tag) => [tag, tag]));

  const selectableTags = [...slugToTag.values()].filter(
    (tag) => !filterTagsSet.has(tag)
  );

  return (
    <SearchContext.Provider
      value={{
        isSearching,

        selectableTags,
        tagUsageCounts,
        projects,

        displayedProjects,

        searchString,
        setSearchString,

        filterTags,
        addFilterTag,
        removeFilterTag,
        clearFilterTags,

        shouldFilterAny,
        setShouldFilterAny,
      }}
    >
      {children}
    </SearchContext.Provider>
  );
};
