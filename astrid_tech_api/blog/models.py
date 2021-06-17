from datetime import datetime
from uuid import uuid4

from django.db.models import Model, TextField, CharField, UUIDField, IntegerField, DateTimeField, URLField, \
    ManyToManyField, ForeignKey, CASCADE, DateField, Max


class Tag(Model):
    short_name = CharField(max_length=32, null=False, blank=False, primary_key=True)
    name = CharField(max_length=32, blank=True)
    color = CharField(max_length=10, blank=True)
    background_color = CharField(max_length=10, blank=True)
    description = TextField(null=True, blank=True)


def default_entry_ordinal():
    return Entry.get_next_ordinal(datetime.now())


class Entry(Model):
    @staticmethod
    def get_next_ordinal(date):
        result = Entry.objects.filter(
            date__exact=date
        ).aggregate(Max('ordinal'))['ordinal__max']

        if result is None:
            return 0
        return result + 1

    uuid = UUIDField(primary_key=True, default=uuid4, editable=False)

    title = CharField(max_length=128, blank=True)
    short_name = CharField(max_length=64, blank=True)
    description = TextField(blank=True)

    created_date = DateTimeField(default=datetime.now)
    """When this entry was originally created. Unless """
    published_date = DateTimeField(default=datetime.now)
    """When this entry was, or will be, published."""
    updated_date = DateTimeField(auto_now=True)
    """When this entry was last updated."""

    date = DateField(default=datetime.now)
    """The date used in the slug."""
    ordinal = IntegerField(null=False, default=default_entry_ordinal)
    """The ordinal entry for the day."""

    reply_to = URLField(blank=True)
    """What this is in reply to."""
    location = URLField(blank=True)
    """What location this was created from, or related to."""
    repost_of = URLField(blank=True)
    """What this is a repost of."""
    tags = ManyToManyField(Tag, blank=True)
    """A list of posts.."""

    content_type = CharField(max_length=127)
    """The content type, as a mimetype."""
    content = TextField(blank=True)
    """The content of this entry."""


class Attachment(Model):
    entry = ForeignKey(Entry, on_delete=CASCADE)
    url = URLField()
    caption = TextField()


class Syndications(Model):
    entry = ForeignKey(Entry, on_delete=CASCADE)
    target = URLField()


class Project(Model):
    uuid = UUIDField(primary_key=True, default=uuid4, editable=False)

    title = TextField()
    description = TextField()
    short_name = TextField()

    start_date = DateTimeField()
    end_date = DateTimeField()
    published_date = DateTimeField()
    updated_date = DateTimeField()

    tags = ManyToManyField(Tag)

    content = TextField()
