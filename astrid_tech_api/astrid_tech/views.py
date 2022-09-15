from django.shortcuts import render


def index(request):
    return render(request, 'astrid_tech/index.html', {})
