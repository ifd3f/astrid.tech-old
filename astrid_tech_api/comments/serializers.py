from rest_framework.fields import CharField
from rest_framework.serializers import ModelSerializer

from comments.models import Comment


class CommentSerializer(ModelSerializer):
    class Meta:
        model = Comment
        fields = [
            'id',
            'slug',
            'time_authored',
            'reply_parent',
            'author_email',
            'author_website',
            'author_name',
            'content_md',
            'content_html',
        ]
        read_only_fields = [
            'content_html',
            'id',
            'time_authored'
        ]
        extra_kwargs = {
            'author_email': {'write_only': True}
        }
