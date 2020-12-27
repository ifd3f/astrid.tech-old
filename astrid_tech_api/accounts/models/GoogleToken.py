from datetime import datetime, timedelta
from typing import Dict

import structlog
from django.db.models import TextField, OneToOneField, CASCADE, DateTimeField, Model
from oauthlib.oauth2 import OAuth2Token
from requests_oauthlib import OAuth2Session

from accounts.google import get_secrets
from accounts.models.IdentityBase import IdentityBase, TokenBase

logger = structlog.get_logger(__name__)


class GoogleAuthAttempt(Model):
    state = TextField(null=False, primary_key=True)
    time_registered = DateTimeField(null=False, auto_now_add=True)


class GoogleToken(TokenBase):
    access_token = TextField(null=False)
    refresh_token = TextField(null=False)
    token_type = TextField(null=False)
    time_expires = DateTimeField(null=False)

    @classmethod
    def from_token(cls, token: OAuth2Token):
        obj = GoogleToken(time_registered=datetime.now())
        obj.token = token
        return obj

    def save_token(self, token):
        self.token = token

    @property
    def token(self):
        return {
            'access_token': self.access_token,
            'refresh_token': self.refresh_token,
            'token_type': self.token_type,
            'expires_in': (self.time_expires - datetime.now()).total_seconds()
        }

    @token.setter
    def token(self, value):
        self.access_token = value['access_token']
        self.refresh_token = value['refresh_token']
        self.token_type = value['token_type']
        self.time_expires = datetime.now() + timedelta(seconds=float(value['expires_in']))

    @property
    def session(self):
        return OAuth2Session(
            get_secrets()['web']['client_id'],
            token=self.token,
            auto_refresh_url="https://www.googleapis.com/oauth2/v4/token",
            token_updater=self.save_token
        )


class GoogleIdentity(IdentityBase):
    google_id = TextField(primary_key=True)
    email = TextField(null=False)
    token = OneToOneField(
        GoogleToken,
        on_delete=CASCADE,
        null=False
    )

    @classmethod
    def create(cls, token: GoogleToken, profile: Dict[str, str]):
        return GoogleIdentity(
            token=token,
            google_id=profile['id'],
            email=profile['email'],
            time_registered=datetime.now()
        )
