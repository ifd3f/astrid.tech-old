import React, { createContext, ReactNode, useContext, useState } from "react"
import { CommentData } from "src/astrid-tech-api"
import { useAPI } from "../APIProvider"

type CommentDataContextData = {
  comments: CommentData[] | null
  isFetching: boolean
  slug: string
  refreshComments: () => Promise<void>
}

const CommentDataContext = createContext(null as CommentDataContextData)

export type CommentDataProviderProps = {
  children: ReactNode
  slug: string
}

export const CommentDataProvider = ({ slug, children }) => {
  const { api } = useAPI()
  const [comments, setComments] = useState<CommentData[] | null>(null)
  const [isFetching, setFetching] = useState(false)

  return (
    <CommentDataContext.Provider
      value={{
        comments,
        isFetching,
        slug,
        refreshComments: async () => {
          const response = await api.getComments(slug)
          setComments(response.data)
        },
      }}
    >
      {children}
    </CommentDataContext.Provider>
  )
}

export const useCommentData = () => useContext(CommentDataContext)
