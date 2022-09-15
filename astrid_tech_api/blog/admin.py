from django.contrib import admin

from blog.models import Entry, Project, Tag, Syndication, Attachment, SyndicationTarget, UploadedFile

admin.site.register(Attachment)
admin.site.register(Entry)
admin.site.register(Project)
admin.site.register(Syndication)
admin.site.register(SyndicationTarget)
admin.site.register(Tag)
admin.site.register(UploadedFile)
