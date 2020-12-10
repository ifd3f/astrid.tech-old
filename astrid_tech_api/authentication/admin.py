from django.contrib import admin

from authentication.models import OAuthIdentity


class OAuthIdentityAdmin(admin.ModelAdmin):
    pass

admin.site.register(OAuthIdentity, OAuthIdentityAdmin)
