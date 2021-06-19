import json
from datetime import datetime, date

import pytz
from django.contrib.auth import get_user_model
from django.contrib.auth.models import Permission
from django.test import TestCase
from freezegun import freeze_time

from blog.models import Entry, SyndicationTarget

EXISTING_MP_SYNDICATE_FORM = {
    'h': 'entry',
    'name': "Syndicating to a Mastodon instance that is enabled owo",
    'mp-syndicate-to': [
        'https://example@twitter.com'
    ]
}

DISABLED_MP_SYNDICATE_FORM = {
    'h': 'entry',
    'name': "Syndicating to a Mastodon instance that is disabled uwu",
    'mp-syndicate-to': [
        'https://not_my@twitter.com'
    ]
}

NONEXISTENT_MP_SYNDICATE_FORM = {
    'h': 'entry',
    'name': "Syndicating to a Mastodon instance that doesn't exist owo",
    'mp-syndicate-to': [
        'https://nobody@mastodon.example.com'
    ]
}

EMPTY_DATE = datetime(2012, 1, 14, 3, 21, 34, 0, pytz.timezone('Asia/Dubai'))
EXPECTED_EMPTY_DATE = date(2012, 1, 13)

OCCUPIED_DATE = datetime(2012, 1, 13, 3, 21, 34, 0, pytz.timezone('Asia/Dubai'))
EXPECTED_OCCUPIED_DATE = date(2012, 1, 12)


# noinspection PyAttributeOutsideInit
class SyndicationTest:
    def set_up_syndication_targets(self):
        self.syn_target_1 = SyndicationTarget.objects.create(
            id='https://example@twitter.com',
            name='My cool twitter account that everyone should follow'
        )
        self.disabled_syn_target = SyndicationTarget.objects.create(
            id='https://not_my@twitter.com',
            name='My uncool twitter account that should never be used',
            enabled=False
        )


class MicropubEndpointTests(TestCase, SyndicationTest):
    @freeze_time(OCCUPIED_DATE)
    def setUp(self):
        self.disallowed_user = get_user_model().objects.create_user(username='stranger', password='7812')
        self.allowed_user = get_user_model().objects.create_user(username='myself', password='12345')
        self.allowed_user.user_permissions.add(Permission.objects.get(codename='add_entry'))

        self.set_up_syndication_targets()

        self.existing_entry = Entry(
            content="I wrote something here but I might write another thing later"
        ).set_all_dates(OCCUPIED_DATE)
        self.existing_entry.save()

    def post(self, **params):
        return self.client.post('/api/micropub/', params)

    def post_and_assert_status(self, expected_status_code=202, **params):
        response = self.post(**params)
        self.assertEqual(expected_status_code, response.status_code, msg=response.content)
        return response

    def get(self, **params):
        return self.client.get('/api/micropub/', params)

    def test_post_fails_on_anonymous(self):
        self.client.logout()

        response = self.post()

        self.assertEqual(401, response.status_code, msg=response.content)

    def test_post_fails_on_disallowed_user(self):
        self.client.force_login(self.disallowed_user)

        response = self.post()

        self.assertEqual(403, response.status_code, msg=response.content)

    def test_get_fails_without_q(self):
        self.client.force_login(self.allowed_user)

        response = self.get()

        self.assertEqual(400, response.status_code)
        data = json.loads(response.content)
        self.assertEqual('invalid_request', data['error'])

    def test_post_fails_without_h(self):
        self.client.force_login(self.allowed_user)

        response = self.post()

        self.assertEqual(400, response.status_code)
        data = json.loads(response.content)
        self.assertEqual('invalid_request', data['error'])

    def test_get_syndication_targets(self):
        self.client.force_login(self.allowed_user)

        response = self.get(q='syndicate-to')

        self.assertEqual(200, response.status_code, msg=response.content)
        data = json.loads(response.content)
        self.assertIn(self.syn_target_1.micropub_syndication_target, data['syndicate-to'])
        self.assertNotIn(self.disabled_syn_target.micropub_syndication_target, data['syndicate-to'])

    def test_get_config(self):
        pass

    def test_get_source(self):
        pass

    @freeze_time(EMPTY_DATE)
    def test_create_valid_entry_1(self):
        self.client.force_login(self.allowed_user)
        form = {
            'h': 'entry',
            'name': 'Hello World!',
            'content': 'My post content dot jay peg',
            'in-reply-to': 'https://example.com',
            'category': ['cpp', 'django', 'python']
        }

        response = self.post_and_assert_status(**form)

        self.assertEqual('https://astrid.tech/2012/01/13/0', response.headers['Link'])
        entry = Entry.objects.get(date=EXPECTED_EMPTY_DATE)
        self.assertEqual(form['name'], entry.title)
        self.assertEqual(form['content'], entry.content)
        self.assertEqual(form['in-reply-to'], entry.reply_to)
        self.assertIsNotNone(entry.tags.get(id='cpp'))
        self.assertIsNotNone(entry.tags.get(id='django'))
        self.assertIsNotNone(entry.tags.get(id='python'))
        self.assertTrue(entry.is_visible())

    @freeze_time(OCCUPIED_DATE)
    def test_create_valid_entry_on_occupied_date(self):
        self.client.force_login(self.allowed_user)
        form = {
            'h': 'entry',
            'content': "I'm writing another note on this date!"
        }

        response = self.post_and_assert_status(**form)

        self.assertEqual('https://astrid.tech/2012/01/12/1', response.headers['Link'])
        entry = Entry.objects.get(date=EXPECTED_OCCUPIED_DATE, ordinal=1)
        self.assertEqual(form['content'], entry.content)
        self.assertTrue(entry.is_visible())

    @freeze_time(EMPTY_DATE)
    def test_create_entry_with_enabled_syndication(self):
        self.client.force_login(self.allowed_user)

        self.post_and_assert_status(**EXISTING_MP_SYNDICATE_FORM)

        entry = Entry.objects.get(date=EXPECTED_EMPTY_DATE)
        syndication = entry.syndication_set.get()
        self.assertEqual(self.syn_target_1, syndication.target)
        self.assertTrue(entry.is_visible())

    @freeze_time(EMPTY_DATE)
    def test_create_entry_with_disabled_syndication(self):
        self.client.force_login(self.allowed_user)

        self.post_and_assert_status(400, **DISABLED_MP_SYNDICATE_FORM)

        self.assertFalse(Entry.objects.filter(date=EXPECTED_EMPTY_DATE).exists())

    @freeze_time(EMPTY_DATE)
    def test_create_entry_with_nonexistent_syndication(self):
        self.client.force_login(self.allowed_user)

        self.post_and_assert_status(400, **NONEXISTENT_MP_SYNDICATE_FORM)

        self.assertFalse(Entry.objects.filter(date=EXPECTED_EMPTY_DATE).exists())
