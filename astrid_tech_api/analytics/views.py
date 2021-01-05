from django.http import HttpResponseNotFound, HttpResponse, FileResponse
from django.shortcuts import render


# Create your views here.
from analytics.models import Resource, Tracker, Hit


def get_media(request, filename):
    track_id = request.GET.get('t')

    resource = Resource.objects.get(name=filename)
    response = HttpResponse(resource.file, content_type='image/png')
    if track_id is None:
        return response
    tracker, _ = Tracker.objects.get_or_create(file=resource, track_id=track_id)
    Hit.create_from_request(tracker, request).save()

    return response
