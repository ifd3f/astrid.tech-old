from django.conf import settings
from django.db import models
from django.db.models import URLField

from authentication.models.IdentityBase import IdentityBase


class IndieWebIdentity(IdentityBase):
    user = models.ForeignKey(
        settings.AUTH_USER_MODEL,
        on_delete=models.CASCADE,
    )
    url = URLField(null=False)
