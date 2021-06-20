from datetime import datetime, timedelta
from urllib.parse import urlparse, parse_qsl, urlencode, urlunparse

import pytz
from django.contrib.auth.decorators import login_required
from django.db import transaction
from django.http import HttpResponse
from django.shortcuts import render, redirect
from django.utils.crypto import get_random_string
from django.views.decorators.http import require_http_methods
from structlog import get_logger

from .models import ConsentRequest

logger = get_logger(__name__)


@login_required
def auth_consent(request):
    if request.method == 'GET':
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
            return HttpResponse(f'cannot authorize {me}', status=400)

        # required fields
        for field in [client_id, redirect_uri, state]:
            if field is None:
                return HttpResponse('missing fields from query', status=400)

        if response_type == 'id':
            pass

        ConsentRequest.objects.filter(client_id=client_id).delete()

        perm_rq = ConsentRequest.objects.create(
            client_id=client_id,
            state=state,
            response_type=response_type,
            redirect_uri=redirect_uri,
            scope=scope,
            expires_at=datetime.now(pytz.utc) + timedelta(minutes=5),
        )
        logger_.info('Rendering form', perm_rq=perm_rq)

        return render(request, 'indieauth/authorize.html', {
            'perm_rq': perm_rq
        })

    if request.method == 'POST':
        redirect_uri = request.POST.get('redirect_uri')
        code = request.POST.get('code')
        client_id = request.POST.get('client_id')
        ConsentRequest.objects.get(client_id=client_id, redirect_uri=redirect_uri, code=code)


@login_required
@require_http_methods(['POST'])
@transaction.atomic
def auth_confirm(request):
    pk = request.POST.get('request_id')
    obj = ConsentRequest.objects.get(pk=pk)
    logger_ = logger.bind(request=obj)

    if obj.expires_at <= datetime.now(pytz.utc):
        obj.delete()
        return HttpResponse('request expired', status=498)

    obj.auth_code = get_random_string(length=64)
    params = {'code': obj.auth_code, 'state': obj.state}
    logger_.info('Adding params to redirect', params=params, redirect_uri=obj.redirect_uri)

    # https://stackoverflow.com/a/2506477 to add state to a query
    parts = urlparse(obj.redirect_uri)
    query = dict(parse_qsl(parts.query))
    query.update(params)
    # noinspection PyProtectedMember
    parts = parts._replace(query=urlencode(query))
    redirect_to = urlunparse(parts)

    obj.delete()
    return redirect(redirect_to)
