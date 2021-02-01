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
            'image'
        ]

        read_only_fields = [
            'image_timestamp',
        ]


