from django.contrib.admin import AdminSite
from django.http import HttpRequest
from django.test import TestCase
from rest_framework.test import APIRequestFactory

from comments.admin import CommentAdmin
from comments.models import Comment
from comments.tests.utils import setup_comment_tree
from comments.views import CommentViewSet


class CommentAdminTestCase(TestCase):
    def setUp(self):
        self.a, self.b, self.c, self.d = setup_comment_tree()

    def test_create_comment(self):
        response = self.client.post(
            '/api/comments/',
            {
                'post': '/foo',
                'content_md': 'test **foo** <script>bar</script>',
                'author_email': 'test@example.com',
                'author_name': 'Tester'
            },
            REMOTE_ADDR='1.2.3.4',
            format='json'
        )
        comment = Comment.objects.get(pk=5)
        self.assertEqual(comment.post, '/foo')
        self.assertEqual(comment.author_name, 'Tester')
        self.assertEqual(comment.author_email, 'test@example.com')
        self.assertEqual(comment.ip_addr, '1.2.3.4')
        self.assertTrue('<script>' not in comment.content_html)
