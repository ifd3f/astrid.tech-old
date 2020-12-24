import axios, { AxiosInstance } from "axios"
import {
  IAuthTokens,
  TokenRefreshRequest,
  useAuthTokenInterceptor,
} from "axios-jwt"

export class AstridTechAPI {
  private axios: AxiosInstance

  constructor(
    private readonly root: string,
    private readonly auth: IAuthTokens
  ) {
    this.axios = axios.create({ baseURL: root })

    const requestRefresh: TokenRefreshRequest = async (
      refreshToken: string
    ): Promise<string> => {
      return (
        await this.axios.post("/api/token/refresh/", { token: refreshToken })
      ).data.access_token
    }
    useAuthTokenInterceptor(axios, { requestRefresh })
  }
}
