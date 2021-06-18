from django.db.models.functions import Now
from rest_framework.permissions import AllowAny
from rest_framework.viewsets import ModelViewSet

from blog.models import Entry


class PublicEntriesViewSet(ModelViewSet):
    queryset = Entry.objects_visible_at(Now())
    permission_classes = [AllowAny]
