from django.urls import include, path
from rest_framework import routers

from accounts import views

router = routers.DefaultRouter()

urlpatterns = [
  #  path('<str:provider>/authorized', views.google_create_account),
]