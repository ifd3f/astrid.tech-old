from rest_framework.serializers import ModelSerializer
from structlog import get_logger

from .models import Entry

logger = get_logger(__name__)


class EntrySerializer(ModelSerializer):
    class Meta:
        model = Entry
