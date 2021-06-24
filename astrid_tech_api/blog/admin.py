from django.contrib import admin

# Register your models here.
from blog.models import Entry, Project, Tag, Syndication

admin.site.register(Tag)
admin.site.register(Syndication)
admin.site.register(Entry)
admin.site.register(Project)
