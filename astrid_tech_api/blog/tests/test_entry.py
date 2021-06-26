from datetime import datetime

from django.db import IntegrityError
from django.db.models.functions import Now
from django.test import TestCase
from freezegun import freeze_time

from blog.models import Entry


class EntryOrdinalTests(TestCase):
    def setUp(self) -> None:
        self.date_1 = datetime(2021, 6, 1)
        self.date_2 = datetime(2021, 6, 8)
        self.post_1 = Entry(slug_name='first-post', ordinal=10).set_all_dates(self.date_1)
        self.post_1.save()

    def test_ordinal_is_zero_for_initial_date(self):
        self.assertEqual(0, Entry.get_next_ordinal(self.date_2))

    def test_ordinal_increments(self):
        self.assertEqual(11, Entry.get_next_ordinal(self.date_1))

    def test_creation_fails_on_duplicate_slug_date_and_ordinal(self):
        with self.assertRaises(IntegrityError):
            entry = Entry(slug_name='second-post', ordinal=10).set_all_dates(self.date_1)
            entry.save()


class EntryStrTests(TestCase):
    def test_str_with_empty_title(self):
        entry = Entry(title='', content='foo bar', ordinal=0).set_all_dates(datetime(2021, 5, 7))
        entry.save()

        self.assertEqual('/2021/05/07/0', str(entry))


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
            Entry.objects_visible_at(Now()).filter(pk=self.existing_post.pk).exists()
        )

    @freeze_time(datetime(2021, 6, 17))
    def test_deleted_post_not_visible(self):
        self.assertFalse(
            Entry.objects_visible_at(Now()).filter(pk=self.deleted_post.pk).exists()
        )

    @freeze_time(datetime(2021, 6, 17))
    def test_future_post_not_visible(self):
        self.assertFalse(
            Entry.objects_visible_at(Now()).filter(pk=self.future_post.pk).exists()
        )

    @freeze_time(datetime(2021, 6, 17))
    def test_null_publish_not_visible(self):
        self.assertFalse(
            Entry.objects_visible_at(Now()).filter(pk=self.null_publish_post.pk).exists()
        )
