from django.conf import settings
from django.db import models
from django.db.models import URLField, Model


class IndieAuthIdentity(Model):
    user = models.ForeignKey(
        settings.AUTH_USER_MODEL,
        on_delete=models.CASCADE,
    )
    url = URLField(null=False)
