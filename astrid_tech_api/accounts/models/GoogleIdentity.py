import os

import requests
from django.db.models import TextField
from django.db.models import URLField, EmailField
from google.auth.transport import requests
from google.oauth2 import id_token

from accounts.models.IdentityBase import IdentityBase


class GoogleIdentity(IdentityBase):
    email = EmailField(null=False, primary_key=True)
    google_user_id = TextField(null=False)

    @classmethod
    def from_token(cls, token):
        request = requests.Request()
        id_info = id_token.verify_oauth2_token(token, request, os.getenv('GITHUB_OAUTH_ID'))

        if id_info['iss'] != 'https://accounts.google.com':
            raise ValueError('Wrong issuer.')

        return id_info, GoogleIdentity(
            google_user_id=id_info['sub'],
            email=id_info['email']
        )

