from django.conf import settings
from django.db import models
from django.db.models import Model, DateField, OneToOneField


class IdentityBase(Model):
    class Meta:
        abstract = True

    user = OneToOneField(
        settings.AUTH_USER_MODEL,
        on_delete=models.CASCADE,
        null=True
    )
    registered = DateField(null=False, auto_now_add=True)
