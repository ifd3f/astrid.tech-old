from datetime import datetime

import pytz
from freezegun import freeze_time
from rest_framework.test import APITestCase

from blog.models import Entry, Tag

create_on = datetime(2021, 6, 18, 5, 5, tzinfo=pytz.utc)
retrieve_on = datetime(2021, 6, 18, 5, 6, tzinfo=pytz.utc)


class EntryAPI(APITestCase):
    @freeze_time(create_on)
    def setUp(self):
        self.entries = [
            Entry.objects.create(
                slug_name=f'entry-{i}',
                title=f'Entry #{i}',
                ordinal=i
            )
            for i in range(20)
        ]

        self.prime_tag = Tag.objects.create(id='prime')
        self.even_tag = Tag.objects.create(id='even')

        for i in [2, 3, 5, 7]:
            self.entries[i].tags.add(self.prime_tag)

        for i in [2, 4, 6, 8]:
            self.entries[i].tags.add(self.even_tag)

    @freeze_time(retrieve_on)
    def test_root_returns_posts(self):
        response = self.client.get('/api/entries/')

        self.assertEqual(20, len(response.json()))

    @freeze_time(retrieve_on)
    def test_can_filter_ordinals(self):
        response = self.client.get('/api/entries/', {'ordinal': 4})

        data = response.json()
        self.assertEqual(1, len(data))
        obj, = data
        self.assertEqual('Entry #4', obj['title'])
