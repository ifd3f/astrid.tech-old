from rest_framework.permissions import AllowAny
from rest_framework.viewsets import ModelViewSet

from blog.models import Entry


class PublicEntriesViewSet(ModelViewSet):
    queryset = Entry.objects.all()
    permission_classes = [AllowAny]
