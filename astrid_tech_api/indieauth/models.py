from django.db.models import URLField, Model, OneToOneField, CASCADE
from oauth2_provider.models import Application


class ClientSite(Model):
    client_id = URLField(primary_key=True, null=False, unique=True)

    application = OneToOneField(Application, on_delete=CASCADE)
