from datetime import datetime

from django.db.transaction import atomic
from django.forms import DateTimeField
from rest_framework.fields import ListField
from rest_framework.serializers import Serializer, CharField, URLField, ModelSerializer

from .models import Entry, Syndication


class MicropubEntrySerializer(Serializer):
    name = CharField(required=False)
    summary = CharField(required=False)
    content = CharField(required=False)

    published = DateTimeField(required=False)
    updated = DateTimeField(required=False)
    category = ListField(CharField())

    in_reply_to = URLField(source='in-reply-to', required=False)
    location = URLField(required=False)
    repost_of = URLField(source='repost-of', required=False)

    syndication = ListField(URLField())
    mp_syndicate_to = ListField(URLField(source='mp-syndicate-to'))

    @staticmethod
    @atomic
    def create_entry(validated_data):
        published = validated_data.get('published', datetime.now())
        entry = Entry(
            title=validated_data.get('name'),
            description=validated_data.get('content'),

            created_date=published,
            published_date=published,
            updated_date=validated_data.get('updated'),

            date=published,

            reply_to=validated_data.get('in-reply-to'),
            location=validated_data.get('location'),
            repost_of=validated_data.get('repost-of'),

            content=validated_data.get('content')
        )
        entry.save()
        for url in validated_data.get('syndication', []):
            entry.syndication_set.create(
                Syndication(
                    entry=entry,
                    location=url,
                    status=Syndication.Status.SYNDICATED
                )
            )
        for url in validated_data.get('mp-syndicate-to', []):
            entry.syndication_set.create(
                Syndication(
                    entry=entry,
                    target=url,
                    status=Syndication.Status.SCHEDULED
                )
            )
        for category in validated_data.get('category', []):
            entry.tags.get_or_create(
                short_name=category
            )
        return entry


class EntrySerializer(ModelSerializer):
    class Meta:
        model = Entry
