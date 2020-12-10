const axios = require("axios")
const oauth = require("axios-oauth-client")

class AstridTechAPI {
  async foo() {
    const getAuthorizationCode = oauth.client(axios.create(), {
      url: "https://oauth.com/2.0/token",
      grant_type: "authorization_code",
      client_id: "foo",
      client_secret: "bar",
      redirect_uri: "...",
      code: "...",
      scope: "baz",
    })

    const auth = await getAuthorizationCode()
  }
}
