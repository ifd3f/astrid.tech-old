from datetime import datetime

import structlog
from rest_framework.decorators import action
from rest_framework.exceptions import NotFound
from rest_framework.mixins import ListModelMixin, RetrieveModelMixin, UpdateModelMixin
from rest_framework.parsers import MultiPartParser, FileUploadParser, FormParser
from rest_framework.permissions import AllowAny
from rest_framework.request import Request
from rest_framework.response import Response
from rest_framework.viewsets import GenericViewSet

from printer3d.models import Printer
from printer3d.serializers import PrinterSerializer

logger = structlog.get_logger(__name__)


class PrinterViewSet(ListModelMixin, UpdateModelMixin, RetrieveModelMixin, GenericViewSet):
    serializer_class = PrinterSerializer
    permission_classes = [AllowAny]

    def get_queryset(self):
        return Printer.objects.all()

    @action(detail=True, methods=['get', 'put'], parser_classes=[MultiPartParser], url_path="image.jpg")
    def image(self, request: Request, pk, filename=None):
        logger_ = logger.bind(id=pk, filename=filename)
        logger_.debug("Checking for existing 3D printer")
        printer = self.get_queryset().get(pk=pk)
        if printer is None:
            raise NotFound(pk)

        if request.method == 'GET':
            return Response(printer.image)

        logger_.info("Submission of an image")

        image = request.data['file']
        if image is None:
            raise ValueError("Did not get an image")
        printer.image = image
        printer.image_timestamp = datetime.utcnow()
        printer.save()

        return Response()

    class Meta:
        permissions = [
            ("update_image", "Can update printer images")
        ]
