from rest_framework.fields import SerializerMethodField
from rest_framework.relations import PrimaryKeyRelatedField
from rest_framework.serializers import ModelSerializer
from structlog import get_logger

from .models import Entry, Syndication, Tag, Content

logger = get_logger(__name__)


class ChildSyndicationSerializer(ModelSerializer):
    class Meta:
        model = Syndication
        fields = ['last_updated', 'location']

class ContentSerializer(ModelSerializer):
    class Meta :
        model = Content
        read_only_fields = ['content_html']
        fields = ['body', 'content_type', 'content_html']

class PublicEntryListSerializer(ModelSerializer):
    syndications = SerializerMethodField()
    tags = PrimaryKeyRelatedField(queryset=Tag.objects.all(), many=True)

    def get_syndications(self, obj: Entry):
        objects = obj.syndications.filter(status=Syndication.Status.SYNDICATED)
        return ChildSyndicationSerializer(objects, many=True).data

    class Meta:
        model = Entry
        read_only_fields = ['date', 'ordinal']

        extra_kwargs = {
            'deleted_date': {'write_only': True}
        }
        exclude = ['id', 'content']


class PublicEntryDetailSerializer(PublicEntryListSerializer):
    content = ContentSerializer()

    class Meta:
        model = Entry
        read_only_fields = ['date', 'ordinal']

        extra_kwargs = {
            'deleted_date': {'write_only': True}
        }
        exclude = ['id']


class TagListSerializer(ModelSerializer):
    class Meta:
        model = Tag
        exclude = ['description']


class TagDetailSerializer(ModelSerializer):
    class Meta:
        model = Tag
        fields = '__all__'
