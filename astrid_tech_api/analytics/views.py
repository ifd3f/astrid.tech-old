from django.http import HttpResponseNotFound, HttpResponse, FileResponse
from django.shortcuts import render


# Create your views here.
from analytics.models import Resource, Hit


def get_media(request, filename):
    track_id = request.GET.get('t')

    resource = Resource.objects.get(name=filename)
    response = HttpResponse(resource.file, content_type='image/png')
    Hit.create_from_request(resource, track_id, request).save()

    return response
