from django.db.models import CharField, URLField, DateTimeField, Model


class ConsentRequest(Model):
    client_id = CharField(max_length=128, unique=True)
    redirect_uri = URLField()
    response_type = CharField(max_length=32)
    scope = CharField(max_length=512)
    expires_at = DateTimeField()
    state = CharField(max_length=512)
    auth_code = CharField(max_length=64, default='')

    def __repr__(self):
        return f'ConsentRequest({self.client_id})'
