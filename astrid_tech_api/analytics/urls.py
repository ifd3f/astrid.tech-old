from django.urls import path

from analytics import views

urlpatterns = [
    path('<filename>', views.get_media)
]
