from django.core.management.utils import get_random_secret_key
from rest_framework.decorators import action
from rest_framework.permissions import AllowAny
from rest_framework.response import Response
from rest_framework.viewsets import ModelViewSet
from structlog import get_logger

from comments.models import Comment
from comments.serializers import CommentSerializer


logger = get_logger(__name__)


class CommentViewSet(ModelViewSet):
    serializer_class = CommentSerializer
    permission_classes = [AllowAny]

    def get_queryset(self):
        return Comment.objects.filter(mod_approved=True)

    @action(detail=True, methods=['post'])
    def reply(self, request, pk=None):
        comment = self.get_object()

    def create(self, request, pk=None, *args, **kwargs):
        ser = CommentSerializer(data=request.data)
        ser.is_valid(raise_exception=True)
        comment_data = ser.validated_data

        x_forwarded_for = request.META.get('HTTP_X_FORWARDED_FOR')
        if x_forwarded_for:
            ip = x_forwarded_for.split(',')[0]
        else:
            ip = request.META.get('REMOTE_ADDR')
        logger.debug('Creating comment', comment=comment_data, ip=ip)

        comment = Comment.objects.create(**comment_data, ip_addr=ip)

        return Response(CommentSerializer(comment).data)
