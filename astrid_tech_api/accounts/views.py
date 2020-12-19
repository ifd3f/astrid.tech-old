from collections import namedtuple
from uuid import uuid4

import httplib2
from django.http import HttpRequest
from django.shortcuts import redirect
from rest_framework.decorators import api_view, permission_classes
from rest_framework.permissions import AllowAny
from rest_framework.request import Request
from rest_framework.response import Response

from accounts.google import get_authorization_url, get_authorization_session, get_secrets
from accounts.models import GoogleToken, User
from accounts.models.GoogleToken import GoogleIdentity

PrefillUserData = namedtuple('PrefillUserData', 'name email')
http = httplib2.Http(cache=".cache")


@api_view()
@permission_classes([AllowAny])
def google_link(request: Request):
    session = get_authorization_session()
    token = session.fetch_token(
        "https://www.googleapis.com/oauth2/v4/token",
        client_secret=get_secrets()['web']['client_secret'],
        code=request.query_params['code']
    )
    google_token = GoogleToken.from_token(token)
    google_token.save()
    profile, identity = GoogleIdentity.create(google_token)
    identity.save()

    return Response({
        'name': profile['name'],
        'email': identity.email,
        'integration': {
            'type': 'google',
            'id': identity.google_id
        }
    })


def google_redirect_authorize(request: HttpRequest):
    return redirect(get_authorization_url())


@api_view()
@permission_classes([AllowAny])
def create_account_from_integration(request: Request):
    return
