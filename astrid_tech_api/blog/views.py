from django.http import HttpResponse, JsonResponse
from django.views.decorators.http import require_http_methods
from structlog import get_logger

from blog.models import SyndicationTarget
from blog.serializer import MicropubEntrySerializer

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

            serializer = MicropubEntrySerializer(data=request.POST)

            if not serializer.is_valid():
                logger_.info('Error while creating post', errors=serializer.errors)
                return _invalid_request(serializer.errors)

            entry = MicropubEntrySerializer.create_entry(serializer.validated_data)
            logger_.info('Successfully created post', entry=entry)

            return HttpResponse(
                status=202,
                headers={'Link': 'https://astrid.tech' + entry.slug}
            )
