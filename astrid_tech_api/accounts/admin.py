from django.contrib import admin

from accounts.models import GithubIdentity, GoogleIdentity, IndieWebIdentity


admin.site.register(GithubIdentity)
admin.site.register(GoogleIdentity)
admin.site.register(IndieWebIdentity)
