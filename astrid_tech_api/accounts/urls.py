from django.urls import path, include
from rest_framework.routers import DefaultRouter

from accounts import views
from accounts.views.user import UserViewSet

router = DefaultRouter()
router.register(r'users', UserViewSet, basename='user')

urlpatterns = [
    path('google/link', views.google.link),
    path('google/authorize', views.google.authorization_redirect),
    path('', include(router.urls))
]
