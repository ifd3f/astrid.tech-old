from django.db.models import Model, DateTimeField, CharField, ImageField, ForeignKey, CASCADE, SET_NULL, FloatField, \
    BooleanField


class Printer(Model):
    name = CharField(max_length=50, null=False, blank=False)
    progress = FloatField(default=0.0, null=False, blank=True)
    status = CharField(max_length=32, null=False, blank=True)
    send_enabled = BooleanField(default=False, null=False)
    image_timestamp = DateTimeField(null=True, blank=True)
    image = ImageField(null=True, blank=True)

