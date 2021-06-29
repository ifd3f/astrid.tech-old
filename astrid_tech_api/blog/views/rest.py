from datetime import datetime

import pytz
from django.core.exceptions import BadRequest
from django.utils.decorators import method_decorator
from django.views.decorators.cache import cache_page
from django.views.decorators.vary import vary_on_cookie
from rest_framework.permissions import IsAuthenticatedOrReadOnly
from rest_framework.viewsets import ModelViewSet

from blog.models import Entry, Tag
from blog.serializer import PublicEntryListSerializer, TagDetailSerializer, TagListSerializer, \
    PublicEntryDetailSerializer


class PublicEntriesViewSet(ModelViewSet):
    permission_classes = [IsAuthenticatedOrReadOnly]
    lookup_field = 'uuid'

    def get_queryset(self):
        qs = Entry.objects_visible_at(datetime.now(pytz.utc))

        try:
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
        except ValueError:
            raise BadRequest()

        return qs

    def get_serializer_class(self):
        params = self.request.query_params
        if self.action == 'list':
            if params.get('detailed') is not None:
                return PublicEntryDetailSerializer
            return PublicEntryListSerializer

        return PublicEntryDetailSerializer


class TagsViewSet(ModelViewSet):
    permission_classes = [IsAuthenticatedOrReadOnly]
    queryset = Tag.objects.all()

    def get_serializer_class(self):
        if self.action == 'retrieve':
            return TagDetailSerializer
        return TagListSerializer
