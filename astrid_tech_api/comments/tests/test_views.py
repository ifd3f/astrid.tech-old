import json
from unittest import skip

from django.http import HttpResponse
from django.test import TestCase

from comments.models import Comment, BannedIP, BannedEmail, BannedEmailDomain, Report
from comments.tests.utils import setup_comment_tree

comment_data = {
    'slug': '/foo',
    'author_email': 'test@example.com',
    'author_name': 'Tester',
    'content_md': 'test **foo** <script>bar</script>',
}


class CommentViewsTestCase(TestCase):
    def setUp(self):
        self.a, self.b, self.c, self.d = setup_comment_tree()

    def banned_comment_asserts(self, response):
        self.assertEqual(403, response.status_code)
        self.assertEqual(4, Comment.objects.count())
        self.assertEqual({'detail': 'testing purposes'}, json.loads(response.content))

    def test_can_create_comment(self):
        response = self.client.post(
            '/api/comments/',
            comment_data,
            REMOTE_ADDR='1.2.3.4',
            format='json'
        )

        self.assertEqual(200, response.status_code)
        comment = Comment.objects.get(pk=5)
        self.assertEqual('/foo', comment.slug)
        self.assertEqual('Tester', comment.author_name)
        self.assertEqual('test@example.com', comment.author_email)
        self.assertEqual('1.2.3.4', comment.ip_addr)
        self.assertEqual(0, comment.children.count())
        self.assertNotIn('<script>', comment.content_html)

    def test_can_reply_to_comment(self):
        response = self.client.post(
            '/api/comments/3/reply/',
            comment_data,
            format='json'
        )

        self.assertEqual(200, response.status_code)
        comment = Comment.objects.get(pk=5)
        self.assertEqual(3, comment.reply_parent.pk)
        self.assertEqual(1, Comment.objects.get(pk=3).children.count())

    def test_can_report_comment(self):
        response = self.client.post(
            '/api/comments/3/report/',
            {
                'reason': 'spam test',
                'email': 'foo@example.com'
            },
            format='json'
        )

        self.assertEqual(200, response.status_code)
        report = Report.objects.get()
        self.assertEqual(3, report.target.id)

    def test_banned_ip_cannot_report_comment(self):
        BannedIP.objects.create(ip_addr='1.2.3.4')

        response = self.client.post(
            '/api/comments/3/report/',
            {
                'reason': 'spam test',
                'email': 'foo@example.com'
            },
            REMOTE_ADDR='1.2.3.4',
            format='json'
        )

        self.assertEqual(403, response.status_code)
        self.assertEqual(0, Report.objects.count())

    @skip("Quarantine NYI on frontend")
    def test_comment_containing_url_is_quarantined(self):
        response = self.client.post(
            '/api/comments/',
            {
                **comment_data,
                'content_md': 'http://google.com might be a spam site'
            },
            REMOTE_ADDR='1.2.3.4',
            format='json'
        )

        self.assertEqual(202, response.status_code)
        comment = Comment.objects.get(pk=5)
        self.assertFalse(comment.mod_approved)

    def test_banned_ip_cannot_post(self):
        BannedIP.objects.create(ip_addr='1.2.3.4', reason='testing purposes')

        response = self.client.post(
            '/api/comments/',
            comment_data,
            REMOTE_ADDR='1.2.3.4',
            format='json'
        )

        self.banned_comment_asserts(response)

    def test_banned_email_cannot_post(self):
        BannedEmail.objects.create(email='spammer@gmail.com', reason='testing purposes')

        response = self.client.post(
            '/api/comments/',
            {
                **comment_data,
                'author_email': 'spammer@gmail.com'
            },
            REMOTE_ADDR='1.2.3.4',
            format='json'
        )

        self.banned_comment_asserts(response)

    def test_banned_email_domain_cannot_post(self):
        BannedEmailDomain.objects.create(domain='evilsite.ru', reason='testing purposes')

        response = self.client.post(
            '/api/comments/',
            {
                **comment_data,
                'author_email': 'spammer@evilsite.ru'
            },
            REMOTE_ADDR='1.2.3.4',
            format='json'
        )

        self.banned_comment_asserts(response)
