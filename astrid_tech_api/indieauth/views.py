from urllib.parse import urlparse, parse_qsl, urlencode, urlunparse

import requests
from django.contrib.auth.decorators import login_required
from django.http import HttpResponse
from django.shortcuts import render
from lxml import etree


def verify_request_uri(client_id: str, request_uri: str) -> bool:
    response = requests.get(client_id)
    if not response.ok:
        raise
    tree = etree.fromstring(response.content)
    tree.findall('link')


@login_required
def indieauth(request):
    if request.method == 'GET':
        client_id = request.GET.get('client_id')
        redirect_uri = request.GET.get('redirect_uri')
        state = request.GET.get('state')
        response_type = request.GET.get('response_type')

        for field in [client_id, redirect_uri, state]:
            if field is None:
                return HttpResponse('missing fields from query', status=400)

        if not verify_request_uri(client_id, redirect_uri):
            return HttpResponse(f'uri {redirect_uri} not registered', status=400)

        code = 32  # TODO

        # https://stackoverflow.com/a/2506477 to add stuff to a query
        parts = urlparse(redirect_uri)
        query = dict(parse_qsl(parts.query))
        query.update({'code': code, 'state': state})
        parts._replace(params=urlencode(query))

        return render(request, 'indieauth/authorize.html', {
            'client_id': client_id,
            'post_endpoint': urlunparse(parts),
        })
