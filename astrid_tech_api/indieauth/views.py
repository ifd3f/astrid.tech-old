from datetime import datetime, timedelta
from urllib.parse import urlparse, parse_qsl, urlencode, urlunparse

import pytz
from django.contrib.auth.decorators import login_required
from django.db import transaction
from django.http import HttpResponse, JsonResponse
from django.shortcuts import render, redirect
from django.views.decorators.http import require_http_methods
from rest_framework.status import *
from structlog import get_logger

from .models import ConsentRequest

logger = get_logger(__name__)


@login_required
def _ask_for_consent(request):
    me = request.GET.get('me')
    client_id = request.GET.get('client_id')
    redirect_uri = request.GET.get('redirect_uri')
    state = request.GET.get('state')
    response_type = request.GET.get('response_type', 'id')
    scope = request.GET.get('scope', '')

    logger_ = logger.bind(me=me, client_id=client_id, redirect_uri=redirect_uri, state=state,
                          response_type=response_type)

    if me not in ['https://astrid.tech', 'https://astrid.tech/']:
        logger_.warn('Unsupported me param')
        return HttpResponse(f'cannot authorize {me}', status=HTTP_400_BAD_REQUEST)

    # required fields
    for field in [client_id, redirect_uri, state]:
        if field is None:
            return HttpResponse('missing fields from query', status=HTTP_400_BAD_REQUEST)

    if response_type == 'id':
        pass

    ConsentRequest.objects.filter(client_id=client_id).delete()

    perm_rq = ConsentRequest.objects.create(
        client_id=client_id,
        me=me,
        state=state,
        response_type=response_type,
        redirect_uri=redirect_uri,
        scope=scope,
        expires_at=datetime.now(pytz.utc) + timedelta(minutes=5),
        confirmed=False
    )
    logger_.info('Rendering form', perm_rq=perm_rq)

    return render(request, 'indieauth/authorize.html', {
        'perm_rq': perm_rq
    })


def _authorize_indieauth(request):
    redirect_uri = request.POST.get('redirect_uri')
    code = request.POST.get('code')
    client_id = request.POST.get('client_id')
    try:
        obj = ConsentRequest.objects.get(client_id=client_id, redirect_uri=redirect_uri, auth_code=code)
    except ConsentRequest.DoesNotExist:
        return HttpResponse('request expired', status=HTTP_401_UNAUTHORIZED)

    if obj.has_expired:
        obj.delete()
        return HttpResponse('request expired', status=HTTP_401_UNAUTHORIZED)

    # TODO create authorization

    obj.delete()

    response = {'me': obj.me}
    if request.headers.get('Accept') == 'application/json':
        return JsonResponse(response, status=HTTP_200_OK)
    else:
        return HttpResponse(urlencode(response), status=HTTP_200_OK)


@require_http_methods(['GET', 'POST'])
def auth_consent(request):
    if request.method == 'GET':
        return _ask_for_consent(request)

    if request.method == 'POST':
        return _authorize_indieauth(request)


@login_required
@require_http_methods(['POST'])
@transaction.atomic
def auth_confirm(request):
    pk = request.POST.get('request_id')
    obj = ConsentRequest.objects.get(pk=pk)
    logger_ = logger.bind(request=obj)

    if obj.expires_at <= datetime.now(pytz.utc):
        obj.delete()
        return HttpResponse('request expired', status=HTTP_401_UNAUTHORIZED)

    obj.confirm()
    obj.save()

    params = {'code': obj.auth_code, 'state': obj.state}
    logger_.info('Adding params to redirect', params=params, redirect_uri=obj.redirect_uri)

    # https://stackoverflow.com/a/2506477 to add state to a query
    parts = urlparse(obj.redirect_uri)
    query = dict(parse_qsl(parts.query))
    query.update(params)
    # noinspection PyProtectedMember
    parts = parts._replace(query=urlencode(query))
    redirect_to = urlunparse(parts)

    return redirect(redirect_to)
