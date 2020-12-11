from django.urls import include, path
from rest_framework import routers

from accounts import views

router = routers.DefaultRouter()

# Wire up our API using automatic URL routing.
# Additionally, we include login URLs for the browsable API.
urlpatterns = [
    path('<str:provider>/authorized', views.auth),
]