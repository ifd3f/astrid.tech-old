from django.db.models import FileField, ForeignKey, SET_NULL, CharField, TextField, GenericIPAddressField, \
    CASCADE, Model, DateTimeField


class Resource(Model):
    file = FileField()
    name = CharField(max_length=32, null=False, primary_key=True, blank=False)


class NamedTracker(Model):
    file = ForeignKey(Resource, on_delete=SET_NULL, null=True)
    track_id = CharField(max_length=8, null=False, blank=False)
    display_name = CharField(max_length=32, null=True, blank=True)

    class Meta:
        unique_together = ('file', 'track_id',)

    def __str__(self):
        if self.display_name is not None:
            return self.display_name
        return 't=' + self.track_id

    @classmethod
    def get_name(cls, file, track_id):
        entry = NamedTracker.objects.get(file=file, track_id=track_id)
        if entry is None:
            return f'{file}?t={track_id}'
        return entry.display_name


class Hit(Model):
    file = ForeignKey(Resource, on_delete=SET_NULL, null=True)
    track_id = CharField(max_length=8, null=True, blank=True)
    time = DateTimeField(auto_now_add=True)

    x_forwarded_for = CharField(max_length=128, null=True)
    remote_addr = GenericIPAddressField(null=True)
    remote_host = CharField(max_length=64, null=True)
    host = CharField(max_length=64, null=True)
    referer = CharField(max_length=64, null=True)
    language = CharField(max_length=32, null=True)
    user_agent = CharField(max_length=64, null=True)

    @property
    def tracker_name(self):
        return NamedTracker.get_name(self.file, self.track_id)

    @classmethod
    def create_from_request(cls, file, track_id, request):
        x_forwarded_for = request.META.get('HTTP_X_FORWARDED_FOR')
        remote_addr = request.META.get('REMOTE_ADDR')
        remote_host = request.META.get('REMOTE_HOST')
        host = request.META.get('HTTP_HOST')
        referer = request.META.get('HTTP_REFERER')
        language = request.META.get('HTTP_ACCEPT_LANGUAGE')
        user_agent = request.META.get('HTTP_USER_AGENT')

        return Hit(
            file=file,
            track_id=track_id,
            x_forwarded_for=x_forwarded_for,
            remote_addr=remote_addr,
            remote_host=remote_host,
            host=host,
            referer=referer,
            language=language,
            user_agent=user_agent
        )

    class Meta:
        index_together = (
            ('file', 'track_id',)
        )

    def __str__(self):
        return f'Hit: {self.tracker_name} @ {self.time}'
