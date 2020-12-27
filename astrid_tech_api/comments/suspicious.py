import re
from itertools import count


def too_many_newlines(max_newlines):
    def validator(comment):
        return sum(1 for c in comment.content_md if c == '\n') > max_newlines

    return validator


def contains_url(comment):
    # https://stackoverflow.com/a/6041965
    match = re.search(r'(http|ftp|https)://([\w_-]+(?:(?:\.[\w_-]+)+))([\w.,@?^=%&:/~+#-]*[\w@?^=%&/~+#-])?',
                      comment.content_md)
    return match is not None
