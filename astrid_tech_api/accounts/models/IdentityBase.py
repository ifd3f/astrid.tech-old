from django.conf import settings
from django.db import models
from django.db.models import Model


class IdentityBase(Model):
    class Meta:
        abstract = True

    user = models.OneToOneField(
        settings.AUTH_USER_MODEL,
        on_delete=models.CASCADE,
        null=True
    )
