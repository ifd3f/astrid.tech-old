from collections import namedtuple
from uuid import uuid4

from rest_framework.decorators import api_view, permission_classes
from rest_framework.permissions import AllowAny
from rest_framework.request import Request
from rest_framework.response import Response

from accounts.models import GoogleIdentity, UserProfile, User

PrefillUserData = namedtuple('PrefillUserData', 'name email')


@api_view()
@permission_classes([AllowAny])
def create_google_integration(request: Request):
    token = request.query_params['token']

    identity = GoogleIdentity.from_token(token)
    identity.save()

    user = User.objects.create_user(
        username=str(uuid4()),
        email=identity.email,
    )
    user.set_unusable_password()
    user.save()


    return Response(PrefillUserData(name=identity.name, email=identity.email))


@api_view()
@permission_classes([AllowAny])
def create_account_from_integration(request: Request):
    return
