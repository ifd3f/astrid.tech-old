import json
from datetime import datetime, timedelta

from django.db.models import TextField, DateField
from oauthlib.oauth2 import OAuth2Token
from requests_oauthlib import OAuth2Session

from accounts.google import get_secrets
from accounts.models.IdentityBase import IdentityBase


class GoogleIdentity(IdentityBase):
    access_token = TextField(null=False)
    refresh_token = TextField(null=False)
    token_type = TextField(null=False)
    time_expires = DateField(null=False)

    @classmethod
    def from_token(cls, token: OAuth2Token):
        obj = GoogleIdentity()
        obj.token = token
        return obj

    def save_token(self, token):
        self.token = token

    @property
    def token(self):
        return {
            'access_token': self.access_token,
            'refresh_token': self.refresh_token,
            'token_type': self.token_type[0],
            'expires_in': (self.time_expires - datetime.now()).total_seconds()
        }

    @token.setter
    def token(self, value):
        self.access_token = value['access_token'],
        self.refresh_token = value['refresh_token'],
        self.token_type = value['token_type'],
        self.time_expires = datetime.now() + timedelta(seconds=float(value['expires_in']))

    @property
    def session(self):
        return OAuth2Session(
            get_secrets()['web']['client_id'],
            token=self.token,
            auto_refresh_url="https://www.googleapis.com/oauth2/v4/token",
            token_updater=self.save_token
        )
