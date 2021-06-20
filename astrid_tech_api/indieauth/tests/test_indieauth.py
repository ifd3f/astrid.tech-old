from datetime import datetime, timedelta

import pytz
from django.contrib.auth import get_user_model
from django.test import TestCase
from freezegun import freeze_time

from indieauth.models import ConsentRequest
from indieauth.util import get_uri_params

working_id_params = dict(
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

    def setup_allowed_user_logged_in(self):
        self.client.force_login(self.allowed_user)

    @freeze_time(confirm_time)
    def setup_asked_consent(self):
        self.setup_allowed_user_logged_in()
        self.client.get('/auth/indieauth', working_id_params)
        return ConsentRequest.objects.get()

    @freeze_time(confirm_time)
    def setup_confirmed_consent_then_logout(self):
        cr = self.setup_asked_consent()
        response = self.client.post('/auth/indieauth/confirm', {'request_id': cr.pk})
        qs = get_uri_params(response.headers['Location'])
        self.client.logout()
        return cr, qs

    def authorize_with_code(self, code, accept='application/json'):
        response = self.client.post(
            '/auth/indieauth',
            {
                'code': code,
                'redirect_uri': working_id_params['redirect_uri'],
                'client_id': working_id_params['client_id']
            },
            HTTP_ACCEPT=accept
        )
        return response

    @freeze_time(confirm_time)
    def test_anonymous_get_endpoint_is_redirected(self):
        self.client.logout()

        response = self.client.get('/auth/indieauth', working_id_params)

        self.assertEqual(302, response.status_code)

    @freeze_time(confirm_time)
    def test_get_endpoint_creates_consent_request(self):
        self.setup_allowed_user_logged_in()

        response = self.client.get('/auth/indieauth', working_id_params)

        self.assertEqual(200, response.status_code)
        cr = ConsentRequest.objects.get()
        self.assertFalse(cr.confirmed)
        self.assertEqual(len(cr.auth_code), 0)
        self.assertEqual(working_id_params['redirect_uri'], cr.redirect_uri)

    @freeze_time(confirm_time)
    def test_post_confirm_confirms_consent_request(self):
        cr = self.setup_asked_consent()

        response = self.client.post('/auth/indieauth/confirm', {'request_id': cr.pk})

        cr = ConsentRequest.objects.get(pk=cr.pk)
        qs = get_uri_params(response.headers['Location'])
        self.assertEqual('param', qs['some'])
        self.assertEqual(working_id_params['state'], qs['state'])
        self.assertEqual(cr.auth_code, qs['code'])
        self.assertEqual(302, response.status_code)
        self.assertTrue(cr.confirmed)
        self.assertGreater(len(cr.auth_code), 0)

    @freeze_time(confirm_time)
    def test_post_endpoint_authorizes_app(self):
        cr, qs = self.setup_confirmed_consent_then_logout()

        response = self.authorize_with_code(qs['code'])

        self.assertFalse(ConsentRequest.objects.filter(pk=cr.pk).exists())
        self.assertEqual(200, response.status_code)
        self.assertEqual({'me': 'https://astrid.tech/'}, response.json())

    @freeze_time(expired_time)
    def test_expired_consent_fails(self):
        cr = self.setup_asked_consent()

        response = self.client.post('/auth/indieauth/confirm', {'request_id': cr.pk})

        self.assertEqual(401, response.status_code)
        self.assertFalse(ConsentRequest.objects.filter(pk=cr.pk).exists())

    @freeze_time(expired_time)
    def test_expired_authorize_fails(self):
        cr, qs = self.setup_confirmed_consent_then_logout()

        response = self.authorize_with_code(qs['code'])

        self.assertEqual(401, response.status_code)
        self.assertFalse(ConsentRequest.objects.filter(pk=cr.pk).exists())

    @freeze_time(confirm_time)
    def test_authorize_without_enough_params_fails(self):
        response = self.client.post(
            '/auth/indieauth',
            {
                'redirect_uri': working_id_params['redirect_uri'],
                'client_id': working_id_params['client_id']
            },
            HTTP_ACCEPT='application/json'
        )

        self.assertEqual(401, response.status_code)

    @freeze_time(confirm_time)
    def test_authorize_with_invalid_code_fails(self):
        self.setup_confirmed_consent_then_logout()

        response = self.authorize_with_code("This ain't a valid code!")

        self.assertEqual(401, response.status_code)

    @freeze_time(confirm_time)
    def test_ask_multiple_consent(self):
        self.setup_allowed_user_logged_in()

        cr_1 = self.setup_asked_consent()
        cr_2 = self.setup_asked_consent()

        self.assertFalse(ConsentRequest.objects.filter(pk=cr_1.pk).exists())
        self.assertTrue(ConsentRequest.objects.filter(pk=cr_2.pk).exists())
