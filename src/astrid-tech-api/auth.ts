import axios, { AxiosInstance } from "axios"
import {
  IAuthTokens,
  TokenRefreshRequest,
  useAuthTokenInterceptor,
} from "axios-jwt"
const BASE_URL = "http://localhost:8001"

export type IAuthResponse = {
  access: string
  refresh: string
}
const refreshEndpoint = `${BASE_URL}/auth/refresh_token`

export const authResponseToAuthTokens = (res: IAuthResponse): IAuthTokens => ({
  accessToken: res.access,
  refreshToken: res.refresh,
})

const requestRefresh: TokenRefreshRequest = async (
  refreshToken: string
): Promise<string> => {
  return (await axios.post(refreshEndpoint, { token: refreshToken })).data
    .access_token
}

export class APISession {
  constructor(private readonly token: IAuthTokens) {}

  static async login(username: string, password: string): Promise<APISession> {
    const res: IAuthResponse = (
      await axios.post("/api/token", { username, password })
    ).data
    return new APISession(authResponseToAuthTokens(res))
  }

  applyToAxios(axios: AxiosInstance): void {
    useAuthTokenInterceptor(axios, { requestRefresh })
  }
}
