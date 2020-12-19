from django.contrib import admin
from django.contrib.auth.admin import UserAdmin

from accounts.models import GithubIdentity, GoogleToken, IndieWebIdentity, User

admin.site.register(GithubIdentity)
admin.site.register(GoogleToken)
admin.site.register(IndieWebIdentity)
admin.site.register(User, UserAdmin)
