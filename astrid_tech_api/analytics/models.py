from django.db.models import FileField, ForeignKey, SET_NULL, CharField, TextField, GenericIPAddressField, \
    CASCADE, Model, DateTimeField


class Resource(Model):
    file = FileField()
    name = CharField(max_length=32, null=False, primary_key=True)


class Tracker(Model):
    file = ForeignKey(Resource, on_delete=SET_NULL, null=True)
    track_id = CharField(max_length=8, null=False, blank=False)
    display_name = TextField(null=True, blank=True)

    class Meta:
        unique_together = ('file', 'track_id',)

    def __str__(self):
        if self.display_name is not None:
            return self.display_name
        return 't=' + self.track_id


class Hit(Model):
    tracker = ForeignKey(Tracker, on_delete=CASCADE, null=False, db_index=True)
    time = DateTimeField(auto_now_add=True)
    ip_addr = GenericIPAddressField(null=False)
    user_agent = TextField(max_length=64, null=False)
