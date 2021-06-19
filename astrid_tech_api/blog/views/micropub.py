from datetime import datetime
from typing import Iterable

import pytz
from django.core.handlers.wsgi import WSGIRequest
from django.db import transaction
from django.http import HttpResponse, JsonResponse, QueryDict
from django.views.decorators.http import require_http_methods
from structlog import get_logger

from blog.models import SyndicationTarget, Entry, Syndication, Tag

logger = get_logger(__name__)
_EMPTY = ['']


def create_syndications(entry: Entry, syndications: Iterable[str]):
    for url in syndications:
        Syndication.objects.create(
            location=url,
            status=Syndication.Status.SYNDICATED,
            entry_id=entry.pk
        )


def create_mp_syndicate_to(entry: Entry, targets: Iterable[str]):
    for uid in targets:
        target = SyndicationTarget.objects.filter(enabled=True).get(id=uid)
        Syndication.objects.create(
            target=target,
            status=Syndication.Status.SCHEDULED,
            entry_id=entry.pk
        )


def create_categories(entry: Entry, categories: Iterable[str]):
    for category in categories:
        tag, _ = Tag.objects.get_or_create(id=category)
        entry.tags.add(tag)


@transaction.atomic
def create_entry_from_query(query: QueryDict):
    published = query.get('published', datetime.now(pytz.utc))
    entry = Entry.objects.create(
        title=query.get('name', ''),
        description=query.get('summary', ''),

        created_date=published,
        published_date=published,

        date=published,

        reply_to=query.get('in-reply-to', ''),
        location=query.get('location', ''),
        repost_of=query.get('repost-of', ''),

        content=query.get('content', ''),
        content_type='text/plain'
    )

    create_syndications(entry, query.getlist('syndication'))
    create_mp_syndicate_to(entry, query.getlist('mp-syndicate-to'))
    create_categories(entry, query.getlist('category'))
    return entry


def parse_mf2_content(content_obj):
    [child] = content_obj
    if isinstance(child, str):  # Plaintext
        return 'text/plain', child
    elif isinstance(child, dict):  # An object, indicating non-plaintext
        [key] = child  # Extract the (hopefully only) key in there
        if key == 'html':
            return 'text/html', child[key]
    raise ValueError(f'Could not parse {repr(content_obj)}')


@transaction.atomic
def create_entry_from_json(obj: dict):
    content_type, content = parse_mf2_content(obj.get('content', _EMPTY))

    published = obj.get('published', datetime.now(pytz.utc))
    entry = Entry.objects.create(
        title=obj.get('name', _EMPTY)[0],
        description=obj.get('summary', _EMPTY),

        created_date=published,
        published_date=published,

        date=published,

        reply_to=obj.get('in-reply-to', _EMPTY),
        location=obj.get('location', _EMPTY),
        repost_of=obj.get('repost-of', _EMPTY),

        content=content
    )

    create_syndications(entry, obj.get('category', []))
    create_mp_syndicate_to(entry, obj.get('mp-syndicate-to', []))
    create_categories(entry, obj.get('category', []))
    return entry


JSON = 'application/json'
FORM = ['application/x-www-form-urlencoded', 'multipart/form-data']


def _invalid_request(info):
    """See https://micropub.spec.indieweb.org/#error-response]"""
    return JsonResponse(
        status=400,
        data={
            'error': 'invalid_request',
            'info': info
        }
    )


def _forbidden():
    """See https://micropub.spec.indieweb.org/#error-response"""
    return JsonResponse(
        status=403,
        data={
            'error': 'forbidden'
        }
    )


def _unauthorized():
    """See https://micropub.spec.indieweb.org/#error-response"""
    return JsonResponse(
        status=401,
        data={
            'error': 'unauthorized'
        }
    )


def _syndication_targets():
    targets = SyndicationTarget.objects.filter(enabled=True)
    return [
        target.micropub_syndication_target
        for target in targets
    ]


def handle_create_json(logger_, request: WSGIRequest):
    [h_type] = request.POST.get('type')

    entry = create_entry_from_json(request.POST)


def handle_create_form(logger_, request: WSGIRequest):
    h_type = request.POST.get('h')
    if h_type is None:
        return _invalid_request('must specify "h"')

    logger_.debug('Decoded h-type', h_type=h_type)

    logger.debug('Creating a new post')
    if request.POST['h'] == 'entry':
        logger_ = logger.bind(form=dict(request.POST))
        logger_.debug('Validating')

        try:
            entry = create_entry_from_query(request.POST)
        except SyndicationTarget.DoesNotExist:
            return _invalid_request('Invalid syndication targets')

        logger_.info('Successfully created post', entry=entry)

        return HttpResponse(
            status=202,
            headers={'Link': 'https://astrid.tech' + entry.slug}
        )

    return _invalid_request(f'unsupported h-type {h_type}')


@require_http_methods(["GET", "POST"])
def micropub(request: WSGIRequest):
    logger_ = logger.bind()

    if request.user.is_anonymous:
        return _unauthorized()

    if not request.user.has_perm('blog.add_entry'):
        return _forbidden()

    if request.method == 'GET':
        # See https://micropub.spec.indieweb.org/#querying

        if 'q' not in request.GET:
            return _invalid_request('must specify "q"')

        if request.GET['q'] == 'syndicate-to':
            return JsonResponse({
                'syndicate-to': _syndication_targets()
            })

    if request.method == 'POST':
        logger_ = logger_.bind(form=dict(request.POST))

        if request.content_type == JSON:
            return handle_create_json(logger_, request)
        elif request.content_type in FORM:
            return handle_create_form(logger_, request)

        return _invalid_request(f'unsupported content-type {request.content_type}')
