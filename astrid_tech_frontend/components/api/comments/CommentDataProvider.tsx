import {
  createContext,
  FC,
  ReactNode,
  useContext,
  useEffect,
  useState,
} from "react";
import { Comment } from "../../../lib/astrid-tech-api";
import { useAPI } from "../APIProvider";

export type CommentFetchState =
  | { status: "initial"; comments: undefined }
  | { status: "fetching"; comments?: Comment[] }
  | { status: "success"; comments: Comment[] }
  | { status: "failure"; reason?: string; comments?: Comment[] };

export type CommentDataContextData = {
  state: CommentFetchState;
  slug: string;
  refreshComments: () => Promise<void>;
};

const CommentDataContext = createContext({} as CommentDataContextData);

export type CommentDataProviderProps = {
  children: ReactNode;
  slug: string;
};

export const CommentDataProvider: FC<CommentDataProviderProps> = ({
  slug,
  children,
}) => {
  const { api } = useAPI();
  const [fetchState, setFetchState] = useState<CommentFetchState>({
    status: "initial",
    comments: undefined,
  });

  const fetchComments = async () => {
    console.log("Refreshing comments");
    setFetchState({ status: "fetching", comments: fetchState.comments });

    let response: Comment[] | undefined;
    try {
      response = await api.getComments(slug);
    } catch (e) {
      console.error("Failure to retrieve comments", e);
      setFetchState({
        status: "failure",
        comments: fetchState.comments,
      });
      return;
    }

    console.log("Got comments", response);
    setFetchState({ status: "success", comments: response });
  };

  const refreshComments = async () => {
    if (fetchState.status == "fetching") return;
    await fetchComments();
  };

  useEffect(() => {
    fetchComments();
  }, [slug]);

  return (
    <CommentDataContext.Provider
      value={{
        state: fetchState,
        slug,
        refreshComments,
      }}
    >
      {children}
    </CommentDataContext.Provider>
  );
};

export const useCommentData = () => useContext(CommentDataContext);
