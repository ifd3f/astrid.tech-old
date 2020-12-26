import re
from itertools import count


def too_many_newlines(max_newlines):
    def validator(value):
        return sum(1 for c in value if c == '\n') > max_newlines

    return validator


def contains_url(value):
    # https://stackoverflow.com/a/6041965
    match = re.search(r'(http|ftp|https)://([\w_-]+(?:(?:\.[\w_-]+)+))([\w.,@?^=%&:/~+#-]*[\w@?^=%&/~+#-])?', value)
    return match is not None
