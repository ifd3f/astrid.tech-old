from rest_framework.decorators import api_view, permission_classes
from rest_framework.permissions import AllowAny
from rest_framework.request import Request


@api_view()
@permission_classes([AllowAny])
def auth(request: Request, provider: str):
    pass
