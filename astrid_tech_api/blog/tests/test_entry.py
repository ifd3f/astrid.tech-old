from datetime import datetime

from django.db import IntegrityError
from django.test import TestCase

from blog.models import Entry


class EntryOrdinalTests(TestCase):
    def setUp(self) -> None:
        self.date_1 = datetime(2021, 6, 1)
        self.date_2 = datetime(2021, 6, 8)
        self.post_1 = Entry(short_name='first-post', ordinal=10).set_all_dates(self.date_1)
        self.post_1.save()

    def test_ordinal_is_zero_for_initial_date(self):
        self.assertEqual(0, Entry.get_next_ordinal(self.date_2))

    def test_ordinal_increments(self):
        self.assertEqual(11, Entry.get_next_ordinal(self.date_1))

    def test_creation_fails_on_duplicate_slug_date_and_ordinal(self):
        with self.assertRaises(IntegrityError):
            entry = Entry(short_name='second-post', ordinal=10).set_all_dates(self.date_1)
            entry.save()
