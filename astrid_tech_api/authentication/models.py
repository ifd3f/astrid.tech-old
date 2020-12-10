from django.conf import settings
from django.db import models
from django.db.models import TextField, DateField, URLField, ForeignKey, Model, IntegerChoices, \
    IntegerField
from django.utils.translation import gettext_lazy as _


class OAuthProvider(IntegerChoices):
    GOOGLE = 0, _('google.com')
    GITHUB = 1, _('github.com')


class OAuthIdentity(Model):
    provider = IntegerField(
        choices=OAuthProvider.choices
    )
    user = ForeignKey(
        settings.AUTH_USER_MODEL,
        on_delete=models.CASCADE,
    )
    refresh_token = TextField(null=False)
    access_token = TextField(null=True)
    expiration = DateField(null=True)


class IndieAuthIdentity(Model):
    user = models.ForeignKey(
        settings.AUTH_USER_MODEL,
        on_delete=models.CASCADE,
    )
    url = URLField(null=False)
