from typing import Union

from rest_framework.decorators import action
from rest_framework.permissions import AllowAny
from rest_framework.request import Request
from rest_framework.response import Response
from rest_framework.viewsets import ModelViewSet
from structlog import get_logger

from comments.models import Comment, BannedIP, Report
from comments.serializers import CommentSerializer, ReportSerializer

logger = get_logger(__name__)


class CommentViewSet(ModelViewSet):
    serializer_class = CommentSerializer
    permission_classes = [AllowAny]

    queryset = Comment.objects.all()

    def get_queryset(self):
        queryset = self.queryset
        query_set = queryset.filter(mod_approved=True).order_by('-time_authored')
        return query_set

    @action(detail=True, methods=['post'])
    def reply(self, request, pk=None):
        comment = self.get_object()
        return CommentViewSet.create_from_request(request, comment)

    @action(detail=True, methods=['post'])
    def report(self, request, pk=None):
        ser = ReportSerializer(data=request)
        ser.is_valid(raise_exception=True)

        comment = self.get_object()
        logger.trace('Creating report', comment=comment, data=ser.validated_data)

        report = Report.objects.create(**ser.validated_data, target=comment)
        return Response(ReportSerializer(report).data)

    def list(self, request: Request, *args, **kwargs):
        slug_filter = request.query_params.get('slug')
        queryset = self.queryset
        if slug_filter is not None:
            queryset = queryset.filter(slug=slug_filter)
        return Response(CommentSerializer(queryset, many=True).data)

    def create(self, request: Request, pk=None, *args, **kwargs):
        return CommentViewSet.create_from_request(request)

    @staticmethod
    def create_from_request(request: Request, reply_parent: Union[Comment, None] = None):
        x_forwarded_for = request.META.get('HTTP_X_FORWARDED_FOR')
        if x_forwarded_for:
            ip = x_forwarded_for.split(',')[0]
        else:
            ip = request.META.get('REMOTE_ADDR')

        ser = CommentSerializer(data=request.data)
        ser.is_valid(raise_exception=True)

        comment_data = ser.validated_data
        comment = Comment(**comment_data, ip_addr=ip, reply_parent=reply_parent)
        logger.debug('Checking if author is banned', comment=comment)

        reason = comment.ban_reason
        if reason is not None:
            logger.info('Banned author attempted to post', comment=comment, reason=reason)
            return Response({
                'error': 'ban',
                'reason': reason
            }, 403)

        comment.save()

        return Response(CommentSerializer(comment).data)
