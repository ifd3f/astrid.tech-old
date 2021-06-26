import json
from datetime import datetime, date
from pathlib import Path

import pytz
from django.contrib.auth import get_user_model
from django.contrib.auth.models import Permission
from django.test import TestCase
from django.urls import reverse
from freezegun import freeze_time

from blog.models import Entry, UploadedFile
from blog.tests import SyndicationTestMixin

TEST_PATH = Path(__file__).parent
IMG1 = TEST_PATH / 'img1.png'
IMG2 = TEST_PATH / 'img2.jpg'

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


class MicropubEndpointTests(TestCase, SyndicationTestMixin):
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

    def post_json_and_assert_status(self, obj, expected_status_code=202):
        response = self.client.post('/api/micropub/', json.dumps(obj), content_type='application/json')
        self.assertEqual(expected_status_code, response.status_code, msg=response.content)
        return response

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
        self.client.force_login(self.allowed_user)

        response = self.client.get(reverse('micropub'), {'q': 'config'})

        self.assertEqual(200, response.status_code, msg=response.content)
        data = response.json()
        self.assertIn(reverse('micropub-media-endpoint'), data['media-endpoint'])

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
        self.assertEqual('text/plain', entry.content_type)
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
        syndication = entry.syndications.get()
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

    @freeze_time(EMPTY_DATE)
    def test_create_json_entry_with_html_content(self):
        form = {
            "type": ["h-entry"],
            "properties": {
                "name": ["Itching: h-event to iCal converter"],
                "content": [
                    {
                        "html": "Now that I've been <a href=\"https://aaronparecki.com/events\">creating a list of "
                                "events</a> on my site using <a href=\"https://p3k.io\">p3k</a>, it would be great if "
                                "I could get a more calendar-like view of that list..."}
                ],
                "category": [
                    "indieweb", "p3k"
                ]
            }
        }
        self.client.force_login(self.allowed_user)

        self.post_json_and_assert_status(form)

        obj = Entry.objects.get(date=EXPECTED_EMPTY_DATE)
        self.assertEqual('text/html', obj.content_type)

    @freeze_time(EMPTY_DATE)
    def test_create_json_entry_with_plain_content(self):
        form = {
            "type": ["h-entry"],
            "properties": {
                "name": ["Itching: h-event to iCal converter"],
                "content": ["testing plaintext do it pls"],
                "category": [
                    "indieweb", "p3k"
                ]
            }
        }
        self.client.force_login(self.allowed_user)

        self.post_json_and_assert_status(form)

        obj = Entry.objects.get(date=EXPECTED_EMPTY_DATE)
        self.assertEqual('text/plain', obj.content_type)

    @freeze_time(EMPTY_DATE)
    def test_create_json_entry_with_attached_photos(self):
        i1_url = "https://example.com/image1.png"
        i2_alt = "some cool caption"
        i2_url = "https://another.site/image2.png"
        form = {
            "type": ["h-entry"],
            "properties": {
                "content": ["testing plaintext do it pls"],
                "photo": [
                    i1_url,
                    {
                        "alt": i2_alt,
                        "value": i2_url
                    }
                ]
            }
        }
        self.client.force_login(self.allowed_user)

        self.post_json_and_assert_status(form)

        obj = Entry.objects.get(date=EXPECTED_EMPTY_DATE)
        [i1, i2] = obj.attachments.all()
        self.assertEqual(i1_url, i1.url)
        self.assertIsNone(i1.caption)
        self.assertEqual(i2_url, i2.url)
        self.assertEqual(i2_alt, i2.caption)


class MediaEndpointTests(TestCase):
    def test_upload_file(self):
        with IMG1.open('rb') as f:
            response = self.client.post('/api/micropub/media', {'file': f})

        self.assertEqual(201, response.status_code, msg=response.content)
        location = response.headers.get('Location')
        self.assertIsNotNone(location)
        self.assertTrue(UploadedFile.objects.filter(name=IMG1.name))

    def test_fails_on_no_file(self):
        response = self.client.post('/api/micropub/media')

        self.assertEqual(400, response.status_code, msg=response.content)
