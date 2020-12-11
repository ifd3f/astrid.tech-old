from django.conf import settings
from django.db import models
from django.db.models import Model, CharField, ImageField


class UserProfile(Model):
    user = models.OneToOneField(
        settings.AUTH_USER_MODEL,
        on_delete=models.CASCADE,
        primary_key=True
    )
    display_name = CharField(max_length=64, null=False)
    picture = ImageField()
