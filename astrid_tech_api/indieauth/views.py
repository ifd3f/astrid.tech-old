from datetime import datetime, timedelta
from urllib.parse import urlparse, parse_qsl, urlencode, urlunparse

import pytz
from django.contrib.auth.decorators import login_required
from django.core.handlers.wsgi import WSGIRequest
from django.db import transaction
from django.http import HttpResponse, JsonResponse
from django.shortcuts import render, redirect
from django.views.decorators.http import require_http_methods
from oauth2_provider.models import Application
from oauth2_provider.views import AuthorizationView
from rest_framework.status import *
from structlog import get_logger

from .models import ClientSite

logger = get_logger(__name__)


# http://127.0.0.1:8001/o/authorize?me=https://astrid.tech/&client_id=https://webapp.example.org/&redirect_uri=https://webapp.example.org/auth/callback&state=1234567890&response_type=code
class IndieAuthView(AuthorizationView):
    def setup_indieauth(self, client_id, redirect_uri):
        """
        Set up the IndieAuth application with the given parameters, or does nothing if it is given an
        invalid combination of parameters.

        :return if they are valid parameters
        """
        # defer to AuthorizationView handler
        if redirect_uri is None or client_id is None:
            return False

        # ensure redirect URI is on the same domain
        if urlparse(client_id).netloc != urlparse(redirect_uri).netloc:
            # TODO check whitelist
            return False

        logger.debug('Ensuring IndieAuth app exists', client_id=client_id)

        with transaction.atomic():
            # Create the app if it doesn't exist and populate fields
            try:
                app = Application.objects.get(client_id=client_id)
            except Application.DoesNotExist:
                app = Application.objects.create(client_id=client_id)
            app.name = f'IndieAuth for {client_id}'
            app.authorization_grant_type = Application.GRANT_AUTHORIZATION_CODE

            # Whitelist this redirect URI if it's not on the list
            if redirect_uri not in app.redirect_uris:
                app.redirect_uris += redirect_uri

            app.save()

            # Create the site if it doesn't exist and populate fields
            try:
                site = ClientSite.objects.get(client_id=client_id)
            except ClientSite.DoesNotExist:
                site = ClientSite.objects.create(client_id=client_id, application=app)

        return True

    def get(self, request, *args, **kwargs):
        client_id = request.GET.get('client_id')
        redirect_uri = request.GET.get('redirect_uri')
        me = request.GET.get('me')
        if me is not None:
            self.setup_indieauth(client_id, redirect_uri)
        return super().get(request, *args, **kwargs)
