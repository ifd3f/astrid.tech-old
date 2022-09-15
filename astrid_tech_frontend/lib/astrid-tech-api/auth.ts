import axios from "axios";
import { IAuthTokens } from "axios-jwt";

export type IAuthResponse = {
  access: string;
  refresh: string;
};

export async function apiLogin(
  root: string,
  username: string,
  password: string
): Promise<IAuthTokens> {
  const res: IAuthResponse = (
    await axios.post("/api/token/", { username, password }, { baseURL: root })
  ).data;
  return {
    accessToken: res.access,
    refreshToken: res.refresh,
  };
}
