import requests
from django.shortcuts import render
from rest_framework.decorators import api_view, permission_classes
from rest_framework.permissions import IsAuthenticated, AllowAny
from rest_framework.request import Request
from rest_framework.views import APIView

from authentication.models.oauth import get_provider, OAuthIdentity


@api_view()
@permission_classes([AllowAny])
def auth(request: Request, provider: str):
    authorization_code = request.query_params['code']

    provider = get_provider(provider)
    token = provider.authorize(authorization_code)
    OAuthIdentity.create_from_token(provider.db_id, token).save()
