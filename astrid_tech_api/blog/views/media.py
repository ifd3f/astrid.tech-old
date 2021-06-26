from uuid import UUID

from django.core.handlers.wsgi import WSGIRequest
from django.http import HttpResponseNotFound
from django.shortcuts import redirect, get_object_or_404

from blog.models import UploadedFile


def single_param_media(request: WSGIRequest, param: str):
    try:
        # Attempt to turn p1 into a UUID
        uuid = UUID(param)
        obj = get_object_or_404(UploadedFile, uuid=uuid)
    except ValueError:
        # P1 is not a UUID, but the file name. Redirect to file that might match.
        obj = get_object_or_404(UploadedFile, name=param)

    return redirect(obj.url, permanent=True)


def exact_media(request: WSGIRequest, uuid: str, name: str):
    try:
        # Attempt to turn p1 into a UUID
        uuid = UUID(uuid)
        obj: UploadedFile = get_object_or_404(UploadedFile, uuid=uuid, name=name)

        return redirect(obj.file.url, permanent=False)
    except ValueError:
        return HttpResponseNotFound()
