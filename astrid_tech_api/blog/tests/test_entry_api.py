from datetime import datetime

from django.test import TestCase
from freezegun import freeze_time

from blog.models import Entry
from blog.views import PublicEntriesViewSet


class PublicEntriesVisibility(TestCase):
    @freeze_time(datetime(2021, 1, 1))
    def setUp(self):
        published = datetime(2021, 1, 1)
        deleted = datetime(2021, 2, 1)
        future = datetime(2021, 10, 1)

        self.deleted_post = Entry(content='Deleted already').set_all_dates(published)
        self.deleted_post.deleted_date = deleted
        self.deleted_post.save()

        self.existing_post = Entry(content='This post should be visible', ordinal=3).set_all_dates(published)
        self.existing_post.save()

        self.future_post = Entry(content='This post is not visible yet').set_all_dates(future)
        self.future_post.save()

        self.null_publish_post = Entry(content='This post is not visible either', published_date=None)
        self.null_publish_post.save()

    @freeze_time(datetime(2021, 6, 17))
    def test_existing_post_is_visible(self):
        self.assertTrue(
            PublicEntriesViewSet.queryset.filter(pk=self.existing_post.pk).exists()
        )

    @freeze_time(datetime(2021, 6, 17))
    def test_deleted_post_not_visible(self):
        self.assertFalse(
            PublicEntriesViewSet.queryset.filter(pk=self.deleted_post.pk).exists()
        )

    @freeze_time(datetime(2021, 6, 17))
    def test_future_post_not_visible(self):
        self.assertFalse(
            PublicEntriesViewSet.queryset.filter(pk=self.future_post.pk).exists()
        )

    @freeze_time(datetime(2021, 6, 17))
    def test_null_publish_not_visible(self):
        self.assertFalse(
            PublicEntriesViewSet.queryset.filter(pk=self.null_publish_post.pk).exists()
        )
