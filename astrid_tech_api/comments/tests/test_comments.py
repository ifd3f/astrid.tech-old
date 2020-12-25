from django.contrib.admin import AdminSite
from django.http import HttpRequest
from django.test import TestCase

from comments.admin import CommentAdmin
from comments.models import Comment


class CommentAdminTestCase(TestCase):
    def setUp(self):
        self.a = Comment.objects.create(
            ip_addr='8.8.8.8',
            locked=False,
            content_md='test foo bar a'
        )
        self.b = Comment.objects.create(
            ip_addr='8.8.8.8',
            locked=False,
            reply_parent=self.a,
            content_md='test foo bar b'
        )
        self.c = Comment.objects.create(
            ip_addr='8.8.8.8',
            locked=False,
            reply_parent=self.b,
            content_md='test foo bar c'
        )
        self.d = Comment.objects.create(
            ip_addr='8.8.8.8',
            locked=False,
            reply_parent=self.b,
            content_md='test foo bar d'
        )
        self.admin = CommentAdmin(Comment, AdminSite())

    def test_lock_thread(self):
        self.admin.lock_thread(HttpRequest(), Comment.objects.filter(pk=self.b.pk))
        self.assertFalse(Comment.objects.get(pk=self.a.pk).locked)
        self.assertTrue(Comment.objects.get(pk=self.b.pk).locked)
        self.assertTrue(Comment.objects.get(pk=self.c.pk).locked)
        self.assertTrue(Comment.objects.get(pk=self.d.pk).locked)

    def test_remove_comment(self):
        self.admin.remove_comment(HttpRequest(), Comment.objects.filter(pk=self.a.pk))
        self.assertTrue(Comment.objects.get(pk=self.a.pk).removed)
