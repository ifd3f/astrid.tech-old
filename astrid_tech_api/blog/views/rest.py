from django.db.models.functions import Now
from rest_framework.permissions import IsAuthenticatedOrReadOnly
from rest_framework.viewsets import ModelViewSet

from blog.models import Entry


class PublicEntriesViewSet(ModelViewSet):
    permission_classes = [IsAuthenticatedOrReadOnly]

    def get_queryset(self):
        qs = Entry.objects_visible_at(Now())

        params = self.request.query_params
        qs = qs.filter(
            year=params.get('year'),
            month=params.get('month'),
            day=params.get('day'),
            ordinal=params.get('ordinal'),
            tags__id=params.getlist('has_tag')
        )

        return qs
