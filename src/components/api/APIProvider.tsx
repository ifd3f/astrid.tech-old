import React, {
  createContext,
  FC,
  ReactNode,
  useContext,
  useState,
} from "react"
import { useCookies } from "react-cookie"
import { apiLogin } from "src/astrid-tech-api/auth"
import { AstridTechAPI } from "../../astrid-tech-api/index"

export type APIContextData = {
  login: (username: string, password: string) => Promise<void>
  logout: () => Promise<void>
  api?: AstridTechAPI
}

const APIContext = createContext(null as APIContextData)

const API_SESSION_COOKIE = "astrid-tech-api-session"
export type APIProviderProps = {
  root: string
  children: ReactNode
}

const tokenSettings = { path: "/", maxAge: 3600 * 24 * 30 }

export const APIProvider: FC<APIProviderProps> = ({ root, children }) => {
  const [api, setAPI] = useState<AstridTechAPI | undefined>(undefined)
  const [cookies, setCookie, removeCookie] = useCookies([API_SESSION_COOKIE])

  const login = async (username: string, password: string) => {
    const session = await apiLogin(root, username, password)
    setCookie(API_SESSION_COOKIE, session)
    setAPI(new AstridTechAPI(root, session))
  }

  const logout = () => {
    setAPI(undefined)
    removeCookie(API_SESSION_COOKIE)
    return Promise.resolve()
  }

  return (
    <APIContext.Provider value={{ api, login, logout }}>
      {children}
    </APIContext.Provider>
  )
}

export const useAPI = () => useContext(APIContext)
