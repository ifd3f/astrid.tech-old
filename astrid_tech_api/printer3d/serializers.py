from rest_framework.serializers import ModelSerializer

from printer3d.models import Printer


class PrinterSerializer(ModelSerializer):
    class Meta:
        model = Printer
        fields = [
            'id',
            'name',
            'progress',
            'status',
            'image',
            'last_updated',
        ]

        read_only_fields = [
            'last_updated',
        ]


