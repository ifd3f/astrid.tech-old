import json

from requests_oauthlib import OAuth2Session

google_secrets = None


def get_secrets():
    global google_secrets
    if google_secrets is None:
        with open('secrets/google_api.json') as f:
            google_secrets = json.load(f)
    return google_secrets


def get_authorization_session():
    return OAuth2Session(
        get_secrets()['web']['client_id'],
        redirect_uri='http://localhost:8000/auth/google/link',
        scope=[
            'https://www.googleapis.com/auth/userinfo.profile',
            'https://www.googleapis.com/auth/userinfo.email',
            'openid'
        ]
    )


def get_authorization_url():
    return get_authorization_session().authorization_url(
        "https://accounts.google.com/o/oauth2/v2/auth", access_type='offline', prompt='select_account')
