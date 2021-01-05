from django.contrib import admin

from analytics.models import Resource, Tracker, Hit

admin.site.register(Resource)
admin.site.register(Tracker)
admin.site.register(Hit)
