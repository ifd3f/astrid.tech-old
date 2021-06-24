from oauth2_provider.oauth2_validators import OAuth2Validator
from oauthlib.common import Request


class IndieAuthValidator(OAuth2Validator):
    def get_additional_claims(self, request: Request):
        return {
            "me": "https://astrid.tech"
        }
