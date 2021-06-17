from django.db.models import Model, TextField, CharField, UUIDField, IntegerField, DateTimeField, URLField, \
    ManyToManyField, ForeignKey, CASCADE


class Tag(Model):
    short_name = CharField(max_length=32, null=False, blank=False, primary_key=True)
    name = CharField(max_length=32)
    color = CharField(max_length=10)
    background_color = CharField(max_length=10)
    description = TextField(null=True, blank=True)


class Entry(Model):
    uuid = UUIDField(primary_key=True)

    name = TextField()
    summary = TextField()
    short_name = TextField()

    created_date = DateTimeField()
    """When this entry was originally created. Unless """
    published_date = DateTimeField()
    """When this entry was, or will be, published."""
    updated_date = DateTimeField(auto_now=True)
    """When this entry was last updated."""
    ordinal = IntegerField(null=False)
    """The ordinal entry for the day."""

    reply_to = URLField()
    """What this is in reply to."""
    location = URLField()
    repost_of = URLField()
    tags = ManyToManyField(Tag)

    content_type = TextField()
    """The content type, as a mimetype."""
    content = TextField()
    """The content of this entry."""


class Attachment(Model):
    entry = ForeignKey(Entry, on_delete=CASCADE)
    url = URLField()
    caption = TextField()


class Syndications(Model):
    entry = ForeignKey(Entry, on_delete=CASCADE)
    target = URLField()


class Project(Model):
    uuid = UUIDField(primary_key=True)

    title = TextField()
    description = TextField()
    short_name = TextField()

    start_date = DateTimeField()
    end_date = DateTimeField()
    published_date = DateTimeField()
    updated_date = DateTimeField()

    tags = ManyToManyField(Tag)

    content = TextField()
