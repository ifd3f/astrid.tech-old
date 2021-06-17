from django.forms import DateTimeField
from rest_framework.serializers import Serializer, CharField, URLField, ModelSerializer

from blog.models import Entry


class MicropubEntrySerializer(Serializer):
    h = CharField()

    name = CharField()
    summary = CharField()
    content = CharField()

    published = DateTimeField()
    updated = DateTimeField()
    category = CharField(many=True)

    in_reply_to = URLField(source='in-reply-to')
    repost_of = URLField(source='repost-of')

    syndication = URLField(many=True)
    mp_syndicate_to = URLField(source='mp-syndicate-to', many=True)


class EntrySerializer(ModelSerializer):
    class Meta:
        model = Entry
