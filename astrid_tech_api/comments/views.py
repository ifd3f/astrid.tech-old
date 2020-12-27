from typing import Union

from django.forms import model_to_dict
from rest_framework.decorators import action
from rest_framework.exceptions import PermissionDenied
from rest_framework.permissions import AllowAny
from rest_framework.request import Request
from rest_framework.response import Response
from rest_framework.viewsets import ModelViewSet
from structlog import get_logger

from comments.models import Comment, BannedIP, Report
from comments.serializers import CommentSerializer, ReportSerializer
from comments.suspicious import too_many_newlines, contains_url

logger = get_logger(__name__)

suspiscion_validators = [
    too_many_newlines(20),
    contains_url
]


def get_request_ip(request: Request):
    x_forwarded_for = request.META.get('HTTP_X_FORWARDED_FOR')
    if x_forwarded_for:
        return x_forwarded_for.split(',')[0]
    else:
        return request.META.get('REMOTE_ADDR')


class CommentViewSet(ModelViewSet):
    serializer_class = CommentSerializer
    permission_classes = [AllowAny]

    def get_queryset(self):
        queryset = Comment.objects.all()\
            .exclude(mod_approved=False)\
            .order_by('-time_authored')
        return queryset

    @action(detail=True, methods=['post'])
    def reply(self, request, pk):
        logger.debug("Request to reply to comment", pk=pk)
        comment = self.get_queryset().get(pk=pk)
        return CommentViewSet.create_from_request(request, comment)

    @action(detail=True, methods=['post'])
    def report(self, request, pk):
        logger.debug("Request to report comment", pk=pk)

        ser = ReportSerializer(data=request.data)
        ser.is_valid(raise_exception=True)

        comment = self.get_queryset().get(pk=pk)
        ip = get_request_ip(request)
        report = Report(**ser.validated_data, target=comment, ip_addr=ip)
        logger_ = logger.bind(comment=model_to_dict(comment), report=model_to_dict(report))

        logger_.debug('Checking if reporter is banned')
        ban_reason = report.author_ban_reason
        if ban_reason is not None:
            logger_.info('Banned reporter attempted to report')
            raise PermissionDenied(ban_reason)

        report.save()
        logger_.info("Successfully reported comment")

        return Response(ReportSerializer(report).data)

    def list(self, request: Request, *args, **kwargs):
        slug_filter = request.query_params.get('slug')
        queryset = self.get_queryset()
        if slug_filter is not None:
            queryset = queryset.filter(slug=slug_filter)
        return Response(CommentSerializer(queryset, many=True).data)

    def create(self, request: Request, pk=None, *args, **kwargs):
        return CommentViewSet.create_from_request(request)

    @staticmethod
    def create_from_request(request: Request, reply_parent: Union[Comment, None] = None):
        ip = get_request_ip(request)

        ser = CommentSerializer(data=request.data)
        ser.is_valid(raise_exception=True)

        comment_data = ser.validated_data
        comment = Comment(**comment_data, ip_addr=ip, reply_parent=reply_parent)
        if reply_parent is not None:
            comment.slug = reply_parent.slug
        logger_ = logger.bind(comment=model_to_dict(comment))

        logger_.debug('Checking if author is banned')
        reason = comment.ban_reason
        if reason is not None:
            logger_.info('Banned author attempted to post', reason=reason)
            raise PermissionDenied(reason)

        # logger_.debug('Checking if message is suspicious')
        # suspicious = any((f(comment) for f in suspiscion_validators))
        # if suspicious:
        #     logger_.info('Marking message as suspicious')
        #     comment.mod_approved = False

        comment.save()
        logger_.info('Successfully created comment')

        return Response(CommentSerializer(comment).data, 200)
