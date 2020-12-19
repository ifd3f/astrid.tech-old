from django.urls import path
from rest_framework import routers

from accounts import views

router = routers.DefaultRouter()

urlpatterns = [
    path('google/link', views.google_link),
    path('google/authorize', views.google_redirect_authorize),
]