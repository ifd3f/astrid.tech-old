from rest_framework.serializers import ModelSerializer

from comments.models import Comment, Report


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
            'content_html'
        ]
        read_only_fields = [
            'content_html',
            'id',
            'time_authored'
        ]
        extra_kwargs = {
            'author_email': {'write_only': True}
        }

    def to_representation(self, instance: Comment):
        result = super().to_representation(instance)
        if instance.removed:
            result['content_html'] = '<p>[removed]</p>'
        return result


class ReportSerializer(ModelSerializer):
    class Meta:
        model = Report
        fields = [
            'target',
            'email',
            'reason'
        ]
        read_only_fields = [
            'target'
        ]
