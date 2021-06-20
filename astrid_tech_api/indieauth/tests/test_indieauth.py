from datetime import datetime, timedelta

import pytz
from django.contrib.auth import get_user_model
from django.test import TestCase
from freezegun import freeze_time

from indieauth.models import ConsentRequest
from indieauth.util import get_uri_params

working_params = dict(
    me='https://astrid.tech/',
    client_id='https://webapp.example.org/',
    redirect_uri='https://webapp.example.org/auth/callback?some=param',
    state='1234567890',
    response_type='id'
)

confirm_time = datetime(2021, 6, 20, 11, 2, 15, tzinfo=pytz.utc)
expired_time = confirm_time + timedelta(minutes=12)


class TestIndieAuthFlow(TestCase):
    """
    Tests functionality following https://indieweb.org/authorization-endpoint#Creating_an_Authorization_Endpoint
    """

    def setUp(self):
        self.disallowed_user = get_user_model().objects.create_user(username='stranger', password='7812')
        self.allowed_user = get_user_model().objects.create_user(username='myself', password='12345')

    def login_allowed(self):
        self.client.force_login(self.allowed_user)

    @freeze_time(confirm_time)
    def test_get_endpoint_creates_consent_request(self):
        self.login_allowed()

        response = self.client.get('/auth/indieauth', working_params)

        self.assertEqual(200, response.status_code)
        obj = ConsentRequest.objects.get()
        self.assertFalse(obj.confirmed)
        self.assertEqual(len(obj.auth_code), 0)
        self.assertEqual(working_params['redirect_uri'], obj.redirect_uri)

    @freeze_time(confirm_time)
    def setup_get_endpoint(self):
        self.login_allowed()
        self.client.get('/auth/indieauth', working_params)
        return ConsentRequest.objects.get()

    @freeze_time(confirm_time)
    def test_post_confirm_confirms_consent_request(self):
        obj = self.setup_get_endpoint()

        response = self.client.post('/auth/indieauth/confirm', {'request_id': obj.pk})

        obj = ConsentRequest.objects.get(pk=obj.pk)
        qs = get_uri_params(response.headers['Location'])
        self.assertEqual('param', qs['some'])
        self.assertEqual(working_params['state'], qs['state'])
        self.assertEqual(obj.auth_code, qs['code'])
        self.assertEqual(302, response.status_code)
        self.assertTrue(obj.confirmed)
        self.assertGreater(len(obj.auth_code), 0)

    @freeze_time(confirm_time)
    def setup_confirm_consent(self):
        obj = self.setup_get_endpoint()
        response = self.client.post('/auth/indieauth/confirm', {'request_id': obj.pk})
        qs = get_uri_params(response.headers['Location'])
        return qs

    @freeze_time(confirm_time)
    def test_post_endpoint_authorizes_app(self):
        qs = self.setup_confirm_consent()

        response = self.client.post(
            '/auth/indieauth',
            {
                'code': qs['code'],
                'redirect_uri': working_params['redirect_uri'],
                'client_id': working_params['client_id']
            },
            HTTP_ACCEPT='application/json'
        )

        self.assertEqual(200, response.status_code)
        self.assertEqual({'me': 'https://astrid.tech/'}, response.json())
