from django.http import HttpResponse, JsonResponse
from django.views.decorators.http import require_http_methods
from structlog import get_logger

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


@require_http_methods(["GET", "POST"])
def micropub(request):
    if request.method == 'POST':
        logger_ = logger.debug('Creating a new post')

        if request.POST['h'] == 'entry':
            normalized = {
                k: vs[0] if len(vs) == 1 else vs
                for k, vs in request.POST.lists()
            }
            logger_ = logger.bind(normalized=request.POST)
            logger_.debug('Validating')

            serializer = MicropubEntrySerializer(data=normalized)

            if not serializer.is_valid():
                return _invalid_request(serializer.errors)

            entry = MicropubEntrySerializer.create_entry(serializer.validated_data)

            return HttpResponse(
                status=202,
                headers={'Link': 'https://astrid.tech' + entry.slug}
            )
