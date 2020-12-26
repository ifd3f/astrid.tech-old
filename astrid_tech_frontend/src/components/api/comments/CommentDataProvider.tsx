import React, {
  createContext,
  FC,
  ReactNode,
  useContext,
  useEffect,
  useState,
} from "react"
import { CommentData } from "src/astrid-tech-api"
import { useAPI } from "../APIProvider"

type CommentDataContextData = {
  comments: CommentData[] | null
  isFetching: boolean
  slug: string
  refreshComments: () => Promise<void>
}

const CommentDataContext = createContext({} as CommentDataContextData)

export type CommentDataProviderProps = {
  children: ReactNode
  slug: string
}

export const CommentDataProvider: FC<CommentDataProviderProps> = ({
  slug,
  children,
}) => {
  const { api } = useAPI()
  const [comments, setComments] = useState<CommentData[] | null>(null)
  const [isFetching, setFetching] = useState(false)

  const fetchComments = async () => {
    setFetching(true)
    console.log("Refreshing comments")
    const response = await api.getComments(slug)
    console.log("Got comments", response)
    setComments(response.data)
    setFetching(false)
  }

  const refreshComments = async () => {
    if (isFetching) return
    await fetchComments()
  }

  useEffect(() => {
    fetchComments()
  }, [slug])

  return (
    <CommentDataContext.Provider
      value={{
        comments,
        isFetching,
        slug,
        refreshComments,
      }}
    >
      {children}
    </CommentDataContext.Provider>
  )
}

export const useCommentData = () => useContext(CommentDataContext)
