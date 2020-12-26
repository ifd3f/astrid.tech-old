import React, { createContext, ReactNode, useState } from "react"
import { CommentData } from "src/astrid-tech-api"
import { useAPI } from "../APIProvider"

type CommentListContextData = {
  comments: CommentData[] | null
  isFetching: boolean
  refreshComments: () => Promise<void>
}

const CommentListContext = createContext(null as CommentListContextData)

export type CommentListProviderProps = {
  children: ReactNode
  slug: string
}

export const CommentListProvider = ({ slug, children }) => {
  const { api } = useAPI()
  const [comments, setComments] = useState<CommentData[] | null>(null)
  const [isFetching, setFetching] = useState(false)

  return (
    <CommentListContext.Provider
      value={{
        comments,
        isFetching,
        refreshComments: async () => {
          const response = await api.getComments(slug)
          setComments(response.data)
        },
      }}
    >
      {children}
    </CommentListContext.Provider>
  )
}
