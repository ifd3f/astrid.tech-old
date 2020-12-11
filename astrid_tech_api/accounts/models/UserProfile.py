from django.conf import settings
from django.db import models
from django.db.models import TextField, Model


class UserProfile(Model):
    user = models.OneToOneField(
        settings.AUTH_USER_MODEL,
        on_delete=models.CASCADE,
        primary_key=True
    )
    name = TextField(null=False)
    email = TextField(null=False)
