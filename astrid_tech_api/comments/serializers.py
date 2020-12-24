from rest_framework.fields import CharField
from rest_framework.serializers import ModelSerializer

from comments.models import Comment


class CommentSerializer(ModelSerializer):
    class Meta:
        model = Comment
        fields = [
            'id',
            'post',
            'reply_parent',
            'author_email',
            'author_website',
            'author_name',
            'content_md',
            'content_html',
        ]
        read_only_fields = ['content_html', 'id']
        extra_kwargs = {
            'author_email': {'write_only': True}
        }
