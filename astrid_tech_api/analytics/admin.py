from django.contrib import admin

from analytics.models import Resource, Hit, NamedTracker

admin.site.register(Resource)
admin.site.register(NamedTracker)
admin.site.register(Hit)
