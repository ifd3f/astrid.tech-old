import json
from datetime import datetime, date, timedelta
from pathlib import Path

import pytz
from django.contrib.auth import get_user_model
from django.contrib.auth.models import Permission
from django.test import TestCase
from django.urls import reverse
from freezegun import freeze_time
from oauth2_provider.models import AccessToken

from blog.models import Entry, UploadedFile, Syndication, Tag, Content
from blog.tests import SyndicationTestMixin
from indieauth.models import ClientSite

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

SIMPLE_URLENCODED_NOTE = {
    'h': 'entry',
    'content': "I'm writing another note on this date!"
}

EMPTY_DATE = datetime(2012, 1, 14, 3, 21, 34, 0, pytz.timezone('Asia/Dubai'))
EXPECTED_EMPTY_DATE = date(2012, 1, 13)

OCCUPIED_DATE = datetime(2012, 1, 13, 3, 21, 34, 0, pytz.timezone('Asia/Dubai'))
EXPECTED_OCCUPIED_DATE = date(2012, 1, 12)


class MicropubEndpointTests(TestCase, SyndicationTestMixin):
    @freeze_time(OCCUPIED_DATE)
    def setUp(self):
        self.set_up_syndication_targets()

        self.disallowed_user = get_user_model().objects.create_user(username='stranger', password='7812')
        self.allowed_user = get_user_model().objects.create_user(username='myself', password='12345')
        self.allowed_user.user_permissions.add(Permission.objects.get(codename='add_entry'))

        self.existing_entry = Entry(
            content=Content.objects.create(body="I wrote something here but I might write another thing later")
        ).set_all_dates(OCCUPIED_DATE)
        self.existing_entry.save()

        self.client_site = ClientSite.get_or_create_full('https://my-micropub-client.com',
                                                         'https://my-micropub-client.com/redirect')
        self.create_only_token = AccessToken.objects.create(
            user=self.allowed_user,
            token='61237612jkl123',
            application=self.client_site.application,
            scope='create',
            expires=datetime.now(tz=pytz.utc) + timedelta(days=3)
        )

        self.auth_headers = {'HTTP_AUTHORIZATION': f'Bearer {self.create_only_token.token}'}

    def post(self, *args, **kwargs):
        return self.client.post('/api/micropub/', *args, **kwargs)

    def get(self, **params):
        return self.client.get('/api/micropub/', params)

    def post_json_and_assert_status(self, obj, *, expected_status_code=201, with_auth_headers=True, **kwargs):
        return self.post_and_assert_status(obj, expected_status_code=expected_status_code,
                                           with_auth_headers=with_auth_headers, content_type='application/json',
                                           **kwargs)

    def post_and_assert_status(self, *args, expected_status_code=201, with_auth_headers=True, **kwargs):
        if with_auth_headers:
            kwargs = {**kwargs, **self.auth_headers}
        response = self.client.post('/api/micropub/', *args, **kwargs)
        self.assertEqual(expected_status_code, response.status_code, msg=response.content)
        return response

    def test_post_fails_without_token(self):
        response = self.post()

        self.assertEqual(401, response.status_code, msg=response.content)

    @freeze_time(OCCUPIED_DATE)
    def test_get_fails_without_q(self):
        response = self.get()

        self.assertEqual(400, response.status_code)
        data = json.loads(response.content)
        self.assertEqual('invalid_request', data['error'])

    @freeze_time(OCCUPIED_DATE)
    def test_post_fails_without_h(self):
        response = self.post_json_and_assert_status({}, expected_status_code=400)

        data = json.loads(response.content)
        self.assertEqual('invalid_request', data['error'])

    @freeze_time(OCCUPIED_DATE)
    def test_get_syndication_targets(self):
        response = self.get(q='syndicate-to')

        self.assertEqual(200, response.status_code, msg=response.content)
        data = json.loads(response.content)
        self.assertIn(self.syn_target_1.micropub_syndication_target, data['syndicate-to'])
        self.assertNotIn(self.disabled_syn_target.micropub_syndication_target, data['syndicate-to'])

    def test_get_config(self):
        response = self.client.get(reverse('micropub'), {'q': 'config'})

        self.assertEqual(200, response.status_code, msg=response.content)
        data = response.json()
        self.assertIn(reverse('micropub-media-endpoint'), data['media-endpoint'])

    @freeze_time(OCCUPIED_DATE)
    def test_can_create_with_params_token(self):
        response = self.post_and_assert_status(
            {**SIMPLE_URLENCODED_NOTE, 'access_token': self.create_only_token.token},
            with_auth_headers=False
        )

        self.assertEqual('https://astrid.tech/2012/01/12/1', response.headers['Location'])

    @freeze_time(OCCUPIED_DATE)
    def test_can_create_with_header_token(self):
        response = self.post_and_assert_status(SIMPLE_URLENCODED_NOTE, **self.auth_headers)

        self.assertEqual('https://astrid.tech/2012/01/12/1', response.headers['Location'])

    @freeze_time(EMPTY_DATE)
    def test_create_valid_entry_1(self):
        form = {
            'h': 'entry',
            'name': 'Hello World!',
            'content': 'My post content dot jay peg',
            'in-reply-to': 'https://example.com',
            'category': ['cpp', 'django', 'python']
        }

        response = self.post_and_assert_status(form)

        self.assertEqual('https://astrid.tech/2012/01/13/0', response.headers['Location'])
        entry = Entry.objects.get(date=EXPECTED_EMPTY_DATE)
        self.assertEqual(form['name'], entry.title)
        self.assertEqual(form['content'], entry.content.body)
        self.assertEqual('text/plain', entry.content.content_type)
        self.assertEqual(form['in-reply-to'], entry.reply_to)
        self.assertIsNotNone(entry.tags.get(id='cpp'))
        self.assertIsNotNone(entry.tags.get(id='django'))
        self.assertIsNotNone(entry.tags.get(id='python'))
        self.assertTrue(entry.is_visible())
        self.assertFalse(entry.repost_of)
        self.assertFalse(entry.location)

    @freeze_time(EMPTY_DATE)
    def test_create_valid_entry_2(self):
        form = {
            'type': ['h-entry'],
            'properties': {
                'name': ['Down-shifting and filtering a signal'],
                'summary': ['Because the FPGA is fast, but not *that* fast'],
                'content': ['Insert content here'],
                'category': ['notes', 'computer-engineering', 'python', 'jupyter',
                             'fpgas', 'numpy', 'matplotlib', 'dsp', 'math',
                             'scikit-learn'],
                'created': ['2020-06-18'],
                'published': ['2020-06-18'],
                'photo': []
            }
        }

        self.post_json_and_assert_status(form)

        self.assertTrue(Tag.objects.filter(id="computer-engineering").exists())
        self.assertFalse(Syndication.objects.exists())

    @freeze_time(EMPTY_DATE)
    def test_create_valid_commonmark_entry(self):
        content = 'Insert content here'
        form = {
            'type': ['h-entry'],
            'properties': {
                'name': ['Down-shifting and filtering a signal'],
                'summary': ['Because the FPGA is fast, but not *that* fast'],
                'content': [{'commonmark': content}],
            }
        }

        self.post_json_and_assert_status(form)

        entry = Entry.objects.get(date=EXPECTED_EMPTY_DATE, ordinal=0)
        self.assertFalse(Syndication.objects.exists())
        self.assertEqual(entry.content.body, content)

    @freeze_time(OCCUPIED_DATE)
    def test_create_valid_entry_on_occupied_date(self):
        form = SIMPLE_URLENCODED_NOTE

        response = self.post_and_assert_status(form)

        self.assertEqual('https://astrid.tech/2012/01/12/1', response.headers['Location'])
        entry = Entry.objects.get(date=EXPECTED_OCCUPIED_DATE, ordinal=1)
        self.assertEqual(form['content'], entry.content.body)
        self.assertTrue(entry.is_visible())
        self.assertFalse(entry.repost_of)
        self.assertFalse(entry.location)

    @freeze_time(EMPTY_DATE)
    def test_create_entry_with_enabled_syndication(self):
        self.post_and_assert_status(EXISTING_MP_SYNDICATE_FORM)

        entry = Entry.objects.get(date=EXPECTED_EMPTY_DATE)
        syndication = entry.syndications.get()
        self.assertEqual(self.syn_target_1, syndication.target)
        self.assertTrue(entry.is_visible())

    @freeze_time(EMPTY_DATE)
    def test_create_entry_with_disabled_syndication(self):
        self.post_and_assert_status(DISABLED_MP_SYNDICATE_FORM, expected_status_code=400)

        self.assertFalse(Entry.objects.filter(date=EXPECTED_EMPTY_DATE).exists())

    @freeze_time(EMPTY_DATE)
    def test_create_entry_with_nonexistent_syndication(self):
        self.post_and_assert_status(NONEXISTENT_MP_SYNDICATE_FORM, expected_status_code=400)

        self.assertFalse(Entry.objects.filter(date=EXPECTED_EMPTY_DATE).exists())

    @freeze_time(EMPTY_DATE)
    def test_create_json_entry_with_html_content(self):
        form = {
            "type": ["h-entry"],
            "properties": {
                "name": ["Itching: h-event to iCal converter"],
                "content": [{
                    "html": "Now that I've been <a href=\"https://aaronparecki.com/events\">creating a list of "
                            "events</a> on my site using <a href=\"https://p3k.io\">p3k</a>, it would be great if "
                            "I could get a more calendar-like view of that list..."
                }],
                "category": [
                    "indieweb", "p3k"
                ],
                "repost-of": []
            }
        }

        self.post_json_and_assert_status(form)

        obj = Entry.objects.get(date=EXPECTED_EMPTY_DATE)
        self.assertEqual('text/html', obj.content.content_type)
        self.assertFalse(obj.repost_of)

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

        self.post_json_and_assert_status(form)

        obj = Entry.objects.get(date=EXPECTED_EMPTY_DATE)
        self.assertEqual('text/plain', obj.content.content_type)

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
