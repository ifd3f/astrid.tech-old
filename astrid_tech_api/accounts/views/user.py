from rest_framework.permissions import AllowAny
from rest_framework.request import Request
from rest_framework.response import Response
from rest_framework.viewsets import ModelViewSet

from accounts.models import User
from accounts.serializers import UserSerializer, CreateUserForm


class UserViewSet(ModelViewSet):
    serializer_class = UserSerializer
    queryset = User.objects.all()

    def get_permissions(self):
        if self.action in ('list', 'retrieve', 'create'):
            permission_classes = [AllowAny]
        else:
            permission_classes = [AllowAny]
        return [permission() for permission in permission_classes]

    def create(self, request: Request, **kwargs):
        form = CreateUserForm(data=request.data)
        if form.is_valid(raise_exception=True):
            serializer = UserSerializer(form.save())
            return Response(serializer.data)
