import os

import requests
from django.db.models import TextField
from django.db.models import URLField, EmailField
from google.auth.transport import requests
from google.oauth2 import id_token

from accounts.models.IdentityBase import IdentityBase


class GithubIdentity(IdentityBase):
    pass
