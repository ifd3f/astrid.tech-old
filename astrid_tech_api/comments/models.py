import bleach
from django.core.validators import MinLengthValidator
from django.db.models import Model, EmailField, URLField, CharField, ForeignKey, SET_NULL, CASCADE, BooleanField, \
    GenericIPAddressField, TextField, DateField, DateTimeField
from markdown import Markdown

markdown = Markdown()


def validate_max_parents(count):
    def validator(value, n=count - 1):
        parent = value.reply_parent
        if parent is None:
            return True
        return n > 0 and validator(parent, n - 1)

    return validator


class BanList(Model):
    class Meta:
        abstract = True

    reason = CharField(max_length=140, default='')
    time_banned = DateTimeField(null=False, auto_now_add=True)


class BannedIP(BanList):
    ip_addr = GenericIPAddressField(primary_key=True, null=False)

    class Meta:
        verbose_name = 'banned IP'

    def __str__(self):
        return f'{self.ip_addr} ({self.reason})'


class BannedEmail(BanList):
    email = EmailField(primary_key=True, null=False)

    def __str__(self):
        return f'{self.email} ({self.reason})'


class BannedEmailDomain(BanList):
    domain = TextField(primary_key=True, null=False)

    def __str__(self):
        return f'{self.domain} ({self.reason})'


class Comment(Model):
    slug = CharField(max_length=100, null=False, db_index=True)
    ip_addr = GenericIPAddressField(verbose_name='IP address', null=False)
    reply_parent = ForeignKey('Comment', null=True, on_delete=CASCADE, validators=[validate_max_parents],
                              related_name='children', blank=True)
    notify_author = BooleanField(default=False)

    time_authored = DateTimeField(null=False, auto_now_add=True)

    mod_approved = BooleanField(default=True)
    removed = BooleanField(default=False)
    locked = BooleanField(default=False)

    author_website = URLField(null=True, blank=True)
    author_email = EmailField(null=False)
    author_name = CharField(max_length=32, null=True, blank=True)

    content_md = TextField(max_length=1000, null=False, validators=[MinLengthValidator(10)], blank=True)
    content_html = TextField(null=False, default='', blank=True)

    @property
    def visible(self):
        return self.mod_approved and not self.removed

    @property
    def can_reply_to(self):
        return self.visible and not self.locked

    def save(self, **kwargs):
        self.content_html = bleach.clean(markdown.convert(self.content_md))
        super().save()

    @property
    def flags(self):
        flags = []
        if self.mod_approved:
            flags.append('a')
        if self.locked:
            flags.append('l')
        if self.removed:
            flags.append('r')
        return ''.join(flags)

    def __str__(self):
        return f'#{self.id} [{self.flags}] re:{self.slug} by {self.author_name} <{self.author_email}>'


class Report(Model):
    target = ForeignKey(Comment, on_delete=CASCADE, null=False)
    email = EmailField(null=True)
    reason = CharField(max_length=140)

    def __str__(self):
        return f'Report for #{self.target} ({self.reason})'
