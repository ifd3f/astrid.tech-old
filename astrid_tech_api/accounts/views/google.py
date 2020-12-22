from collections import namedtuple

import httplib2
import structlog
from django.http import HttpRequest
from django.shortcuts import redirect
from rest_framework.decorators import api_view, permission_classes
from rest_framework.exceptions import ParseError
from rest_framework.permissions import AllowAny
from rest_framework.request import Request
from rest_framework.response import Response

from accounts.google import get_authorization_url, get_authorization_session, get_secrets
from accounts.models import GoogleIdentity, GoogleToken, GoogleAuthAttempt

logger = structlog.get_logger(__name__)

PrefillUserData = namedtuple('PrefillUserData', 'name email')
http = httplib2.Http(cache=".cache")


@api_view()
@permission_classes([AllowAny])
def link(request: Request):
    state = request.query_params['state']
    logger.debug('User redirected from OAuth', state=state)

    attempt = GoogleAuthAttempt.objects.filter(state=state)
    if not attempt.exists():
        logger.warn('Redirected user has invalid state', state=state)
        raise ParseError(
            detail="Invalid state"
        )
    attempt.delete()

    logger.debug('Fetching OAuth 2.0 token', state=state)
    session = get_authorization_session()
    token = session.fetch_token(
        "https://www.googleapis.com/oauth2/v4/token",
        client_secret=get_secrets()['web']['client_secret'],
        code=request.query_params['code']
    )
    google_token = GoogleToken.from_token(token)
    google_token.save()

    logger.debug('Fetching user profile information', state=state)
    profile = google_token.session.get('https://www.googleapis.com/oauth2/v1/userinfo').json()
    logger.debug("Received user profile information", state=state, profile=profile)

    identity = GoogleIdentity.create(google_token, profile)
    identity.save()

    return Response({
        'username': profile['name'],
        'email': identity.email,
        'authentication': {
            'type': 'google',
            'id': identity.google_id
        }
    })


def authorization_redirect(request: HttpRequest):
    url, state = get_authorization_url()
    logger.debug('Request for Google auth', state=state)
    GoogleAuthAttempt(state=state).save()
    return redirect(url)
