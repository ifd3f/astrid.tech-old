from datetime import datetime

from django.db import transaction
from django.forms import DateTimeField
from rest_framework.fields import ListField
from rest_framework.serializers import Serializer, CharField, URLField, ModelSerializer
from structlog import get_logger

from .models import Entry, Syndication, Tag, SyndicationTarget

logger = get_logger(__name__)


class MicropubEntrySerializer(Serializer):
    name = CharField(required=False)
    summary = CharField(required=False)
    content = CharField(required=False)

    published = DateTimeField(required=False)
    updated = DateTimeField(required=False)
    category = ListField(child=CharField(), required=False)

    in_reply_to = URLField(source='in-reply-to', required=False)
    location = URLField(required=False)
    repost_of = URLField(source='repost-of', required=False)

    syndication = ListField(child=URLField(), required=False)
    mp_syndicate_to = ListField(source='mp-syndicate-to', child=URLField(), required=False)

    @staticmethod
    @transaction.atomic
    def create_entry(validated_data):
        published = validated_data.get('published', datetime.now())
        entry = Entry.objects.create(
            title=validated_data.get('name', ''),
            description=validated_data.get('summary', ''),

            created_date=published,
            published_date=published,

            date=published,

            reply_to=validated_data.get('in-reply-to', ''),
            location=validated_data.get('location', ''),
            repost_of=validated_data.get('repost-of', ''),

            content=validated_data.get('content', '')
        )

        for url in validated_data.get('syndication', []):
            Syndication.objects.create(
                location=url,
                status=Syndication.Status.SYNDICATED,
                entry_id=entry.pk
            )
        for url in validated_data.get('mp-syndicate-to', []):
            target = SyndicationTarget.objects.filter(enabled=True).get(id=url)
            Syndication.objects.create(
                target=target,
                status=Syndication.Status.SCHEDULED,
                entry_id=entry.pk
            )
        for category in validated_data.get('category', []):
            tag, _ = Tag.objects.get_or_create(short_name=category)
            entry.tags.add(tag)
        return entry

    @staticmethod
    def from_entry(entry: Entry):
        pass


class EntrySerializer(ModelSerializer):
    class Meta:
        model = Entry
