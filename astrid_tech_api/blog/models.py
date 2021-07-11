from datetime import datetime, timezone
from uuid import uuid4

import pytz
from django.core.validators import URLValidator
from django.db.models import Model, TextField, CharField, UUIDField, IntegerField, DateTimeField, URLField, \
    ManyToManyField, ForeignKey, CASCADE, DateField, Max, TextChoices, BooleanField, RESTRICT, Q, QuerySet, FileField, \
    OneToOneField


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


class Tag(Model):
    id = CharField(max_length=32, null=False, blank=False, primary_key=True)
    name = CharField(max_length=32, blank=True)
    color = CharField(max_length=10, blank=True, null=True)
    background_color = CharField(max_length=10, blank=True, null=True)
    description = TextField(null=True, blank=True)

    content_warning = BooleanField(default=False, null=False)

    def save(self, *args, **kwargs):
        if not self.name:
            self.name = self.id
        if not self.color:
            self.color = None
        if not self.background_color:
            self.background_color = None
        super().save(*args, **kwargs)

    def __str__(self):
        return self.id


class UploadedFile(Model):
    name = CharField(max_length=64, blank=False)
    content_type = CharField(max_length=64)
    uuid = UUIDField(default=uuid4, null=False, blank=True)
    created = DateTimeField(auto_now_add=True)
    updated = DateTimeField(auto_now=True)
    file = FileField()

    @property
    def url(self):
        return f'/assets/{self.uuid}/{self.name}'

    class Meta:
        unique_together = ('uuid', 'name')


def default_entry_ordinal():
    return Entry.get_next_ordinal()


def utc_now():
    return datetime.now(pytz.utc)


class Content(Model):
    """The content of a page."""

    content_type = CharField(max_length=127, default='text/plain')
    """The content type, as a mimetype."""
    body = TextField(blank=True, default='')
    """The content of this entry."""
    content_html = TextField(blank=True, default='')
    """The content, rendered as HTML."""

    class Status(TextChoices):
        from django.utils.translation import gettext_lazy as _
        SYNDICATED = 'SY', _('Syndicated')
        SCHEDULED = 'SC', _('Scheduled')
        ERROR = 'ER', _('Error')

    status = CharField(max_length=2, choices=Status.choices, null=False, default=Status.SCHEDULED)
    """The status of this content."""

    def save(self, force_insert=False, force_update=False, using=None,
             update_fields=None):
        if self.content_type == 'text/markdown':
            pass
        elif self.content_type == 'text/html':
            # HTML is just HTML.
            self.content_html = self.body
        else:
            # Treat unknown content types as plaintext
            self.content_html = f"<pre>{self.body}</pre>"
        super(Content, self).save(force_insert, force_update, using, update_fields)


class Entry(Model):
    @staticmethod
    def get_next_ordinal(date=None):
        """
        Next ordinal for the given date, or None if no ordinal.
        """
        if date is None:
            date = datetime.utcnow()

        result = Entry.objects.filter(
            date__exact=date.astimezone(pytz.utc)  # On the given date
        ).aggregate(Max('ordinal'))['ordinal__max']  # Take the biggest ordinal

        if result is None:
            return 0  # First post of the date
        return result + 1

    uuid = UUIDField(unique=True, default=uuid4, editable=False)

    title = CharField(max_length=128, blank=True, null=True)
    slug_name = CharField(max_length=64, blank=True, null=True)
    description = TextField(blank=True, null=True, default='')

    created_date = DateTimeField(default=utc_now, blank=True)
    """When this entry was originally created. Usually the same as the published date."""
    published_date = DateTimeField(default=utc_now, null=True, blank=True)
    """When this entry was, or will be, published."""
    updated_date = DateTimeField(auto_now=True)
    """When this entry was last updated."""
    deleted_date = DateTimeField(null=True, blank=True)
    """When this entry was deleted or is scheduled to be deleted, or None if it is not deleted."""

    date = DateField(default=utc_now)
    """The date used in the slug."""
    ordinal = IntegerField(null=False, default=default_entry_ordinal)
    """The ordinal entry for the day."""

    reply_to = URLField(blank=True, null=True, validators=[URLValidator()])
    """What this is in reply to."""
    location = URLField(blank=True, null=True, validators=[URLValidator(["mailto"])])
    """What location this was created from, or related to."""
    repost_of = URLField(blank=True, null=True)
    """What this is a repost of."""
    tags = ManyToManyField(Tag, blank=True)
    """Tags this entry is associated with."""

    content = OneToOneField(Content, on_delete=RESTRICT, null=True, default=None)
    """The content of this page."""

    @staticmethod
    def objects_visible_at(dt) -> 'QuerySet[Entry]':
        return Entry.objects.filter(
            Q(published_date__isnull=False) & Q(published_date__lte=dt) &
            (Q(deleted_date__isnull=True) | Q(deleted_date__gt=dt))
        )

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
        if self.slug_name:
            slug += '/' + self.slug_name
        return slug

    def is_visible_at(self, date):
        date = date.astimezone(pytz.utc)
        if self.published_date is not None and self.published_date.astimezone(pytz.utc) <= date:
            return True
        return self.deleted_date is None or self.deleted_date.astimezone(pytz.utc) >= date

    def is_visible(self):
        return self.is_visible_at(datetime.now())

    def __str__(self):
        if self.title:
            return self.title
        return self.slug

    class Meta:
        unique_together = ('date', 'ordinal')


class Attachment(Model):
    entry = ForeignKey(Entry, on_delete=CASCADE, null=False, blank=False, related_name='attachments')
    """The entry this attachment is attached to."""
    index = IntegerField(null=False)
    """A value for keeping track of upload order."""
    url = URLField(null=False, blank=False)
    """Where this attachment points to."""
    content_type = CharField(max_length=128, null=False, blank=False)
    """The type of content this is."""
    caption = TextField(blank=True, null=True)
    """A caption for this attachment."""
    spoiler = BooleanField(default=False, null=False)
    """If this contains sensitive content."""


class Syndication(Model):
    entry = ForeignKey(Entry, on_delete=CASCADE, null=False, blank=False, related_name='syndications')
    """The entry this syndication is associated with."""
    target = ForeignKey(SyndicationTarget, on_delete=RESTRICT, null=True, blank=True, related_name='syndications')
    """Where to syndicate this object to, or None if we do not need to syndicate it."""

    last_updated = DateTimeField(auto_now=True)
    """The last time this syndication was updated."""
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
    slug_name = TextField()

    start_date = DateTimeField()
    end_date = DateTimeField()
    published_date = DateTimeField()
    updated_date = DateTimeField()

    tags = ManyToManyField(Tag)

    content = OneToOneField(Content, on_delete=RESTRICT, null=False)
    """The content of this page."""
