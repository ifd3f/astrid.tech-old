from django.http import HttpResponse
from structlog import get_logger

from analytics.models import Resource, Hit


logger = get_logger(__name__)


def get_media(request, filename):
    track_id = request.GET.get('t')

    logger_ = logger.bind(filename=filename, track_id=track_id)

    resource = Resource.objects.get(name=filename)
    response = HttpResponse(resource.file, content_type='image/png')
    logger_.info("Recording hit @ tracked file")

    # noinspection PyBroadException
    try:
        Hit.create_from_request(resource, track_id, request).save()
    except Exception as e:
        logger_.warn('Error while recording hit', error=e)

    return response
