from django.db import transaction
from django.db.models import URLField, Model, OneToOneField, CASCADE
from oauth2_provider.models import Application


class ClientSite(Model):
    client_id = URLField(primary_key=True, null=False, unique=True)

    application = OneToOneField(Application, on_delete=CASCADE)

    @staticmethod
    @transaction.atomic
    def get_or_create_full(client_id, redirect_uri):
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

        return site
