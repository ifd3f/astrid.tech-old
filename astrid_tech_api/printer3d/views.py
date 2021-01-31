from datetime import datetime

import structlog
from rest_framework.decorators import action
from rest_framework.exceptions import NotFound
from rest_framework.mixins import ListModelMixin, RetrieveModelMixin
from rest_framework.permissions import AllowAny
from rest_framework.request import Request
from rest_framework.response import Response
from rest_framework.viewsets import GenericViewSet

from printer3d.models import Printer
from printer3d.serializers import PrinterSerializer

logger = structlog.get_logger(__name__)


class PrinterViewSet(ListModelMixin, RetrieveModelMixin, GenericViewSet):
    serializer_class = PrinterSerializer
    permission_classes = [AllowAny]

    def get_queryset(self):
        return Printer.objects.all()

    @action(detail=True, methods=['post'])
    def image(self, request: Request, pk):
        logger_ = logger.bind(id=pk)
        logger_.info("Submission of an image")
        logger_.debug("Checking for existing 3D printer")
        printer = self.get_queryset().get(pk=pk)
        if printer is None:
            raise NotFound(pk)

        image = request.data['file']
        if image is None:
            raise ValueError("Did not get an image")
        printer.image = image
        printer.image_timestamp = datetime.now()
        printer.save()

        return Response()

    class Meta:
        permissions = [
            ("update_image", "Can update printer images")
        ]
