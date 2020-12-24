import bleach
from django.core.validators import MinLengthValidator
from django.db.models import Model, EmailField, URLField, CharField, ForeignKey, SET_NULL, CASCADE, BooleanField, \
    GenericIPAddressField, TextField
from markdown import Markdown

markdown = Markdown()


def validate_max_parents(count):
    def validator(value, n=count - 1):
        parent = value.reply_parent
        if parent is None:
            return True
        return n > 0 and validator(parent, n - 1)
    return validator


class BannedIP(Model):
    ip_addr = GenericIPAddressField(null=False, primary_key=True)
    reason = CharField(max_length=140)


class BannedEmail(Model):
    email = EmailField(null=False)
    reason = CharField(max_length=140)


class Comment(Model):
    post = CharField(max_length=100, null=False, db_index=True)
    ip_addr = GenericIPAddressField(null=False)
    reply_parent = ForeignKey('Comment', null=True, on_delete=SET_NULL, validators=[validate_max_parents])
    published = BooleanField(default=True)

    author_website = URLField(null=True)
    author_email = EmailField(null=False)
    author_name = CharField(max_length=32, null=True)

    content_md = CharField(max_length=1000, null=False, validators=[MinLengthValidator(10)])
    content_html = TextField(null=False, default='')

    def save(self, **kwargs):
        self.content_html = bleach.clean(markdown.convert(self.content_md))
        super().save()


class Report(Model):
    target = ForeignKey(Comment, on_delete=CASCADE, null=False)
    reason = CharField(max_length=140)
