import { createContext, FC, ReactNode, useContext, useState } from "react";
import { useCookies } from "react-cookie";
import { apiLogin } from "../../lib/astrid-tech-api/auth";
import { AstridTechAPI } from "../../lib/astrid-tech-api/index";

export type APIContextData = {
  login: (username: string, password: string) => Promise<void>;
  logout: () => Promise<void>;
  api: AstridTechAPI;
};

const APIContext = createContext({} as APIContextData);

const API_SESSION_COOKIE = "astrid-tech-api-session";

const tokenSettings = { path: "/", maxAge: 3600 * 24 * 30 };

export type APIProviderProps = {
  root: string;
  children: ReactNode;
};

export const APIProvider: FC<APIProviderProps> = ({ root, children }) => {
  const [api, setAPI] = useState<AstridTechAPI>(new AstridTechAPI(root));
  const [cookies, setCookie, removeCookie] = useCookies([API_SESSION_COOKIE]);

  const login = async (username: string, password: string) => {
    const session = await apiLogin(root, username, password);
    setCookie(API_SESSION_COOKIE, session);
    setAPI(new AstridTechAPI(root));
  };

  const logout = () => {
    removeCookie(API_SESSION_COOKIE);
    return Promise.resolve();
  };

  return (
    <APIContext.Provider value={{ api, login, logout }}>
      {children}
    </APIContext.Provider>
  );
};

export const useAPI = () => useContext(APIContext);
