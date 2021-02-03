from django.contrib.admin import AdminSite
from django.http import HttpRequest
from rest_framework.test import APITestCase

from comments.admin import CommentAdmin
from comments.models import Comment
from comments.tests.utils import setup_comment_tree


class CommentAdminTestCase(APITestCase):
    def setUp(self):
        self.a, self.b, self.c, self.d = setup_comment_tree()
        self.admin = CommentAdmin(Comment, AdminSite())

    def test_lock_thread(self):
        self.admin.lock_thread(HttpRequest(), Comment.objects.filter(pk=self.b.pk))
        self.assertFalse(Comment.objects.get(pk=self.a.pk).locked)
        self.assertTrue(Comment.objects.get(pk=self.b.pk).locked)
        self.assertTrue(Comment.objects.get(pk=self.c.pk).locked)
        self.assertTrue(Comment.objects.get(pk=self.d.pk).locked)

    def test_remove_comment(self):
        self.admin.remove_comment(HttpRequest(), Comment.objects.filter(pk=self.a.pk))

        removed_comment = Comment.objects.get(pk=self.a.pk)
        self.assertTrue(removed_comment.removed)
