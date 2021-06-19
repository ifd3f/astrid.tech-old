from rest_framework.serializers import ModelSerializer
from structlog import get_logger

from .models import Entry

logger = get_logger(__name__)


class PublicEntrySerializer(ModelSerializer):
    class Meta:
        model = Entry
        read_only_fields = ['date', 'ordinal']

        extra_kwargs = {
            'deleted_date': {'write_only': True}
        }
        exclude = ['id']
