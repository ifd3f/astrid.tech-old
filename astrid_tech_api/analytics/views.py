from django.http import HttpResponseNotFound, HttpResponse, FileResponse
from django.shortcuts import render


# Create your views here.
from analytics.models import Resource, Tracker, Hit


def get_request_ip(request):
    x_forwarded_for = request.META.get('HTTP_X_FORWARDED_FOR')
    if x_forwarded_for:
        return x_forwarded_for.split(',')[0]
    else:
        return request.META.get('REMOTE_ADDR')


def get_media(request, filename):
    track_id = request.GET.get('t')
    ip = get_request_ip(request)
    ua = request.META.get('HTTP_USER_AGENT')

    resource = Resource.objects.get(name=filename)
    response = HttpResponse(resource.file, content_type='image/png')
    if track_id is None:
        return response
    tracker, _ = Tracker.objects.get_or_create(file=resource, track_id=track_id)
    Hit.objects.create(tracker=tracker, ip_addr=ip, user_agent=ua)

    return response
