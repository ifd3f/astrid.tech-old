from comments.models import Comment


def setup_comment_tree():
    a = Comment.objects.create(
        slug='/test',
        ip_addr='8.8.8.8',
        locked=False,
        content_md='test foo bar a'
    )
    b = Comment.objects.create(
        slug='/test',
        ip_addr='8.8.8.8',
        locked=False,
        reply_parent=a,
        content_md='test foo bar b'
    )
    c = Comment.objects.create(
        slug='/test',
        ip_addr='8.8.8.8',
        locked=False,
        reply_parent=b,
        content_md='test foo bar c'
    )
    d = Comment.objects.create(
        slug='/test',
        ip_addr='8.8.8.8',
        locked=False,
        reply_parent=b,
        content_md='test foo bar d'
    )
    return a, b, c, d
