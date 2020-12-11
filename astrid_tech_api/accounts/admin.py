from django.contrib import admin
from django.contrib.auth.admin import UserAdmin

from accounts.models import GithubIdentity, GoogleIdentity, IndieWebIdentity, User

admin.site.register(GithubIdentity)
admin.site.register(GoogleIdentity)
admin.site.register(IndieWebIdentity)
admin.site.register(User, UserAdmin)
