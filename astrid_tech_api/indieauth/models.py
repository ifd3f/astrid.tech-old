from datetime import datetime

import pytz
from django.db.models import CharField, URLField, DateTimeField, Model, BooleanField
from django.utils.crypto import get_random_string


class ConsentRequest(Model):
    client_id = CharField(max_length=128, unique=True)
    me = CharField(max_length=128, unique=True)
    redirect_uri = URLField()
    response_type = CharField(max_length=32)
    scope = CharField(max_length=512)
    expires_at = DateTimeField()
    state = CharField(max_length=512)
    auth_code = CharField(max_length=64, default='')
    confirmed = BooleanField(default=False)

    def __repr__(self):
        return f'ConsentRequest({self.client_id})'

    def confirm(self):
        self.confirmed = True
        self.auth_code = get_random_string(length=64)

    @property
    def has_expired(self):
        return datetime.now(pytz.utc) >= self.expires_at
