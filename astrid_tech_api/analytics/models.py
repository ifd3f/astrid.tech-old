from django.db.models import FileField, ForeignKey, SET_NULL, CharField, TextField, GenericIPAddressField, \
    CASCADE, Model, DateTimeField


class Resource(Model):
    file = FileField()
    name = CharField(max_length=32, null=False, primary_key=True, blank=False)


class Tracker(Model):
    file = ForeignKey(Resource, on_delete=SET_NULL, null=True)
    track_id = CharField(max_length=8, null=False, blank=False)
    display_name = CharField(max_length=32, null=True, blank=True)

    class Meta:
        unique_together = ('file', 'track_id',)

    def __str__(self):
        if self.display_name is not None:
            return self.display_name
        return 't=' + self.track_id


class Hit(Model):
    tracker = ForeignKey(Tracker, on_delete=CASCADE, null=False, db_index=True)
    time = DateTimeField(auto_now_add=True)

    x_forwarded_for = CharField(max_length=128, null=True)
    remote_addr = GenericIPAddressField(null=True)
    remote_host = CharField(max_length=64, null=True)
    host = CharField(max_length=64, null=True)
    referer = CharField(max_length=64, null=True)
    language = CharField(max_length=32, null=True)
    user_agent = CharField(max_length=64, null=True)

    @classmethod
    def create_from_request(cls, tracker, request):
        x_forwarded_for = request.META.get('HTTP_X_FORWARDED_FOR')
        remote_addr = request.META.get('REMOTE_ADDR')
        remote_host = request.META.get('REMOTE_HOST')
        host = request.META.get('HTTP_HOST')
        referer = request.META.get('HTTP_REFERER')
        language = request.META.get('HTTP_ACCEPT_LANGUAGE')
        user_agent = request.META.get('HTTP_USER_AGENT')

        return Hit(
            tracker=tracker,
            x_forwarded_for=x_forwarded_for,
            remote_addr=remote_addr,
            remote_host=remote_host,
            host=host,
            referer=referer,
            language=language,
            user_agent=user_agent
        )
