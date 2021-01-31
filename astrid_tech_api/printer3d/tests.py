from django.test import TestCase

from printer3d.models import Printer


class CommentViewsTestCase(TestCase):
    def setUp(self):
        self.printer = Printer()