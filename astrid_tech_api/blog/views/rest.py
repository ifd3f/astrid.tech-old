from datetime import datetime

import pytz
from rest_framework.permissions import IsAuthenticatedOrReadOnly
from rest_framework.viewsets import ModelViewSet

from blog.models import Entry, Tag
from blog.serializer import PublicEntrySerializer, TagSerializer


class PublicEntriesViewSet(ModelViewSet):
    permission_classes = [IsAuthenticatedOrReadOnly]
    serializer_class = PublicEntrySerializer
    lookup_field = 'uuid'

    def get_queryset(self):
        qs = Entry.objects_visible_at(datetime.now(pytz.utc))

        params = self.request.query_params

        year = params.get('year')
        if year is not None:
            qs = qs.filter(date__year=year)

        month = params.get('month')
        if month is not None:
            qs = qs.filter(date__month=month)

        day = params.get('day')
        if day is not None:
            qs = qs.filter(date__day=day)

        ordinal = params.get('ordinal')
        if ordinal is not None:
            qs = qs.filter(ordinal=ordinal)

        tags = params.getlist('has_tag')
        for tag in tags:
            qs = qs.filter(tags__id=tag)

        return qs


class TagsViewSet(ModelViewSet):
    permission_classes = [IsAuthenticatedOrReadOnly]
    serializer_class = TagSerializer
    queryset = Tag.objects.all()
