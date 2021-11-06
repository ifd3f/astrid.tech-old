import { createContext, FC, ReactNode, useContext } from "react";
import { useCookies } from "react-cookie";

export type NSFWContextData = {
  enabled: boolean;
  setEnabled: (shouldEnable: boolean) => void;
};

const NSFWContext = createContext({} as NSFWContextData);

export type NSFWProviderProps = {
  root: string;
  children: ReactNode;
};

const NSFW_COOKIE_KEY = "enable-nsfw";

export const NSFWProvider: FC<NSFWContextData> = ({ children }) => {
  const [cookies, setCookie] = useCookies([NSFW_COOKIE_KEY]);

  const enabled = cookies[NSFW_COOKIE_KEY] == "1";
  const setEnabled = (e: boolean) => setCookie(NSFW_COOKIE_KEY, e ? "1" : "0");

  return (
    <NSFWContext.Provider value={{ enabled, setEnabled }}>
      {children}
    </NSFWContext.Provider>
  );
};

export const useNSFW = () => useContext(NSFWContext);
