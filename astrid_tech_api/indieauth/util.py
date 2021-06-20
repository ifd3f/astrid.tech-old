from urllib.parse import urlparse, parse_qsl


def get_uri_params(uri):
    redirect_uri = urlparse(uri)
    return dict(parse_qsl(redirect_uri.query))


