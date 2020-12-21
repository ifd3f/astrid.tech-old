from uuid import uuid4

from django.contrib.auth.models import AbstractUser
from django.db.models import UUIDField


class User(AbstractUser):
    id = UUIDField(primary_key=True, default=uuid4, editable=False)
