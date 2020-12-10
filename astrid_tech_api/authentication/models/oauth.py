import os
from datetime import datetime, timedelta
from typing import Union

from django.conf import settings
from django.db import models
from django.db.models import IntegerChoices, IntegerField, ForeignKey
from django.db.models import TextField, DateField, Model
from django.utils.translation import gettext_lazy as _
from oauthlib.oauth2 import BackendApplicationClient
from requests_oauthlib import OAuth2Session


class OAuthProvider:
    def __init__(self, db_id: int, client_id: str, client_secret: str, subpath_name: str, token_endpoint: str):
        self.db_id = db_id
        self.token_endpoint = token_endpoint
        self.subpath_name = subpath_name
        self.client_secret = client_secret
        self.client_id = client_id
        self.client = BackendApplicationClient(client_id=client_id)

    def authorize(self, code):
        oauth = OAuth2Session(client=self.client)
        return oauth.fetch_token(
            self.token_endpoint,
            client_secret=self.client_secret,
            authorization_response=code
        )

    def __repr__(self):
        return f'OAuthProvider({self.subpath_name}, {self.db_id})'


class OAuthProviderTypes(IntegerChoices):
    GOOGLE = 0, _('google.com')
    GITHUB = 1, _('github.com')

    def get_provider(self):
        return OAuthProviderTypes.PROVIDER_TYPE_TO_PROVIDER[self[0]]


PROVIDERS = [
    OAuthProvider(
        OAuthProviderTypes.GOOGLE,
        client_id=os.getenv('GOOGLE_OAUTH_ID'),
        client_secret=os.getenv('GOOGLE_OAUTH_SECRET'),
        subpath_name='google',
        token_endpoint='https://oauth2.googleapis.com/token'
    ),
    OAuthProvider(
        OAuthProviderTypes.GITHUB,
        client_id=os.getenv('GITHUB_OAUTH_ID'),
        client_secret=os.getenv('GITHUB_OAUTH_SECRET'),
        subpath_name='github',
        token_endpoint='https://github.com/login/oauth/access_token'
    )
]

DB_ID_TO_PROVIDER = {x.db_id: x for x in PROVIDERS}
SUBPATH_TO_PROVIDER = {x.subpath_name: x for x in PROVIDERS}


def get_provider(key: Union[int, str]) -> OAuthProvider:
    if isinstance(key, int):
        return DB_ID_TO_PROVIDER[key]
    if isinstance(key, str):
        return SUBPATH_TO_PROVIDER[key]
    raise TypeError(f'key must be an int or str, got {key}')


class OAuthIdentity(Model):
    provider_code = IntegerField(
        choices=OAuthProviderTypes.choices
    )
    user = ForeignKey(
        settings.AUTH_USER_MODEL,
        on_delete=models.CASCADE,
    )
    refresh_token = TextField(null=False)
    access_token = TextField(null=True)
    expiration = DateField(null=True)

    @classmethod
    def create_from_token(cls, provider: int, token: str):
        return OAuthIdentity(
            refresh_token=token,
            provider_code=provider
        )

    @property
    def provider(self):
        return get_provider(self.provider_code)

    @property
    def token(self):
        return {
            'access_token': self.access_token,
            'refresh_token': self.refresh_token,
            'token_type': 'Bearer',
            'expires_in': (self.expiration - datetime.now()).total_seconds()
        }

    @token.setter
    def token(self, token):
        self.access_token = token['access_token']
        self.refresh_token = token['refresh_token']
        self.expiration = datetime.now() + timedelta(seconds=token['expires_in'])
        self.save()

    def get_session(self):
        provider = self.provider

        def save_token(token):
            self.token = token

        return OAuth2Session(
            client_id=provider.client_id,
            token=self.token,
            auto_refresh_url=provider.token_endpoint,
            token_updater=save_token
        )
