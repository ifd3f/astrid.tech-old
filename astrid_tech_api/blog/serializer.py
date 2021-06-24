from rest_framework.relations import PrimaryKeyRelatedField, StringRelatedField
from rest_framework.relations import PrimaryKeyRelatedField
from rest_framework.serializers import ModelSerializer
from structlog import get_logger

from .models import Entry, Syndication, Tag

logger = get_logger(__name__)


class ChildSyndicationSerializer(ModelSerializer):
    target = StringRelatedField(many=True)

    class Meta:
        model = Syndication
        exclude = ['id', 'entry']


class PublicEntrySerializer(ModelSerializer):
    #syndications = ChildSyndicationSerializer(many=True, read_only=True)
    tags = PrimaryKeyRelatedField(queryset=Tag.objects.all(), many=True)

    class Meta:
        model = Entry
        read_only_fields = ['date', 'ordinal']

        extra_kwargs = {
            'deleted_date': {'write_only': True}
        }
        exclude = ['id']
