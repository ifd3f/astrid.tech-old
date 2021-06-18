from datetime import datetime, timezone
from uuid import uuid4

from django.db.models import Model, TextField, CharField, UUIDField, IntegerField, DateTimeField, URLField, \
    ManyToManyField, ForeignKey, CASCADE, DateField, Max, TextChoices, BooleanField, RESTRICT


class Tag(Model):
    short_name = CharField(max_length=32, null=False, blank=False, unique=True)
    name = CharField(max_length=32, blank=True)
    color = CharField(max_length=10, blank=True)
    background_color = CharField(max_length=10, blank=True)
    description = TextField(null=True, blank=True)

    def __str__(self):
        return self.short_name


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

    uuid = UUIDField(unique=True, default=uuid4, editable=False)

    title = CharField(max_length=128, blank=True, null=True)
    short_name = CharField(max_length=64, blank=True, null=True)
    description = TextField(blank=True, null=True)

    created_date = DateTimeField(default=datetime.now, blank=True)
    """When this entry was originally created. Usually the same as the published date."""
    published_date = DateTimeField(default=datetime.now, null=True, blank=True)
    """When this entry was, or will be, published."""
    updated_date = DateTimeField(auto_now=True)
    """When this entry was last updated."""
    deleted_date = DateTimeField(null=True, blank=True)
    """When this entry was deleted or is scheduled to be deleted, or None if it is not deleted."""

    date = DateField(default=datetime.now)
    """The date used in the slug."""
    ordinal = IntegerField(null=False, default=default_entry_ordinal)
    """The ordinal entry for the day."""

    reply_to = URLField(blank=True, null=True)
    """What this is in reply to."""
    location = URLField(blank=True, null=True)
    """What location this was created from, or related to."""
    repost_of = URLField(blank=True, null=True)
    """What this is a repost of."""
    tags = ManyToManyField(Tag, blank=True)
    """Tags this entry is associated with."""

    content_type = CharField(max_length=127, default='text/markdown')
    """The content type, as a mimetype."""
    content = TextField(blank=True, default='')
    """The content of this entry."""

    def set_all_dates(self, dt: datetime):
        """Helper to set all the date fields to the given date. Mostly useful for testing and little else."""
        self.date = dt.astimezone(timezone.utc)
        self.created_date = dt
        self.published_date = dt
        self.updated_date = dt
        return self

    @property
    def slug(self):
        slug = f'/{self.date.year}/{self.date.month:02}/{self.date.day:02}/{self.ordinal}'
        if self.short_name:
            slug += '/' + self.short_name
        return slug

    def is_visible_at(self, date):
        if self.published_date is not None and self.published_date <= date:
            return True
        return self.deleted_date is None or self.deleted_date >= date

    def __str__(self):
        if self.title is not None:
            return self.title
        return self.slug

    class Meta:
        unique_together = ('date', 'ordinal')


class Attachment(Model):
    entry = ForeignKey(Entry, on_delete=CASCADE, null=False, blank=False)
    """The entry this attachment is attached to."""
    url = URLField(null=False, blank=False)
    """Where this attachment points to."""
    caption = TextField(blank=True, null=True)
    """A caption for this attachment."""
    spoiler = BooleanField(default=False, null=False)
    """If this contains sensitive content."""


class SyndicationTarget(Model):
    id = URLField(primary_key=True, null=False)
    name = CharField(max_length=50, blank=True)
    enabled = BooleanField(default=True)

    def __str__(self):
        if self.name is not None:
            return self.name
        return self.id

    @property
    def micropub_syndication_target(self):
        return {'uid': self.id, 'name': self.name}


class Syndication(Model):
    entry = ForeignKey(Entry, on_delete=CASCADE, null=False, blank=False)
    """The entry this syndication is associated with."""

    last_updated = DateTimeField(auto_now=True)
    """The last time this syndication was updated."""
    target = ForeignKey(SyndicationTarget, on_delete=RESTRICT, null=True, blank=True)
    """Where to syndicate this object to, or None if we do not need to syndicate it."""
    location = URLField(null=True, blank=True)
    """Where this entry was syndicated to, or None if it has not been syndicated there."""

    class Status(TextChoices):
        from django.utils.translation import gettext_lazy as _
        SYNDICATED = 'SY', _('Syndicated')
        SCHEDULED = 'SC', _('Scheduled')
        ERROR = 'ER', _('Error')

    status = CharField(max_length=2, choices=Status.choices, null=False, default=Status.SCHEDULED)
    """The status of this syndication."""

    class Meta:
        unique_together = ('entry', 'target')


class Project(Model):
    uuid = UUIDField(unique=True, default=uuid4, editable=False)

    title = TextField()
    description = TextField()
    short_name = TextField()

    start_date = DateTimeField()
    end_date = DateTimeField()
    published_date = DateTimeField()
    updated_date = DateTimeField()

    tags = ManyToManyField(Tag)

    content = TextField()
