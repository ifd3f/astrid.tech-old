from datetime import datetime

from django.db import transaction
from django.http import HttpResponse, JsonResponse
from django.views.decorators.http import require_http_methods
from structlog import get_logger

from blog.models import SyndicationTarget, Entry, Syndication, Tag

logger = get_logger(__name__)


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
            'error': 'forbidden'
        }
    )


def _syndication_targets():
    targets = SyndicationTarget.objects.filter(enabled=True)
    return [
        target.micropub_syndication_target
        for target in targets
    ]


@transaction.atomic
def create_entry(query):
    published = query.get('published', datetime.now())
    entry = Entry.objects.create(
        title=query.get('name', ''),
        description=query.get('summary', ''),

        created_date=published,
        published_date=published,

        date=published,

        reply_to=query.get('in-reply-to', ''),
        location=query.get('location', ''),
        repost_of=query.get('repost-of', ''),

        content=query.get('content', '')
    )

    for url in query.getlist('syndication'):
        Syndication.objects.create(
            location=url,
            status=Syndication.Status.SYNDICATED,
            entry_id=entry.pk
        )
    for url in query.getlist('mp-syndicate-to'):
        target = SyndicationTarget.objects.filter(enabled=True).get(id=url)
        Syndication.objects.create(
            target=target,
            status=Syndication.Status.SCHEDULED,
            entry_id=entry.pk
        )
    for category in query.getlist('category'):
        tag, _ = Tag.objects.get_or_create(id=category)
        entry.tags.add(tag)
    return entry


@require_http_methods(["GET", "POST"])
def micropub(request):
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
        if 'h' not in request.POST:
            return _invalid_request('must specify "h"')

        logger.debug('Creating a new post')
        if request.POST['h'] == 'entry':
            logger_ = logger.bind(form=dict(request.POST))
            logger_.debug('Validating')

            '''
            if not serializer.is_valid():
                logger_.info('Error while creating post', errors=serializer.errors)
                return _invalid_request(serializer.errors)
            '''

            try:
                entry = create_entry(request.POST)
            except SyndicationTarget.DoesNotExist:
                return _invalid_request('Invalid syndication targets')

            logger_.info('Successfully created post', entry=entry)

            return HttpResponse(
                status=202,
                headers={'Link': 'https://astrid.tech' + entry.slug}
            )
