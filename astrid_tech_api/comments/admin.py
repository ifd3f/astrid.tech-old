from django.contrib import admin
from django.contrib.admin import ModelAdmin
from django.db.models import QuerySet

from comments.models import Comment, Report, BannedEmail, BannedIP


class CommentAdmin(ModelAdmin):
    actions = [
        'lock_thread',
        'remove_comment',
        'ban_email',
        'ban_ip'
    ]

    def lock_thread(self, request, queryset: QuerySet[Comment]):
        comments = []
        stack = list(queryset)
        while len(stack) > 0:
            comment = stack.pop()
            comment.locked = True
            comments.append(comment)

            for c in comment.children:
                stack.append(c)

        Comment.objects.bulk_update(comments)
    lock_thread.short_description = "Lock thread"

    def remove_comment(self, request, queryset: QuerySet[Comment]):
        queryset.update(published=False)
    remove_comment.short_description = "Remove comment (without deleting)"

    def ban_email(self, request, queryset: QuerySet[Comment]):
        BannedEmail.objects.bulk_create([BannedEmail(email=comment.author_email) for comment in queryset])
    ban_email.short_description = "Ban author email"

    def ban_ip(self, request, queryset: QuerySet[Comment]):
        BannedIP.objects.bulk_create([BannedIP(ip_addr=comment.ip_addr) for comment in queryset])
    ban_ip.short_description = "Ban author IP"


admin.site.register(Comment)
admin.site.register(Report)
admin.site.register(BannedEmail)
admin.site.register(BannedIP)

