from django.contrib.auth.models import Permission
from django.http import HttpResponse, JsonResponse
from django.views.decorators.http import require_http_methods
from structlog import get_logger

from blog.models import SyndicationTarget
from blog.serializer import MicropubEntrySerializer

logger = get_logger(__name__)


def _invalid_request(info):
    return JsonResponse(
        status=400,
        data={
            'error': 'invalid_request',
            'info': info
        }
    )


def _forbidden():
    return JsonResponse(
        status=403,
        data={
            'error': 'forbidden'
        }
    )


def _unauthorized():
    return JsonResponse(
        status=401,
        data={
            'error': 'forbidden'
        }
    )



@require_http_methods(["GET", "POST"])
def micropub(request):
    if request.method == 'GET':
        if 'q' not in request.GET:
            return _invalid_request('must specify "q"')

        if request.GET['q'] == 'syndicate-to':
            targets = SyndicationTarget.objects.filter(enabled=True)
            return JsonResponse({
                'syndicate-to': [
                    {'uid': target.id, 'name': target.enabled}
                    for target in targets
                ]
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
