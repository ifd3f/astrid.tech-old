
from django.contrib import admin
from django.urls import path, include
from rest_framework.routers import DefaultRouter
from rest_framework_simplejwt.views import TokenObtainPairView, TokenRefreshView

from comments.views import CommentViewSet
from printer3d.views import PrinterViewSet

router = DefaultRouter()
router.register(r'comments', CommentViewSet, basename='comments')
router.register(r'3dprinter', PrinterViewSet, basename='3dprinter')

urlpatterns = [
    path('admin/', admin.site.urls),
    path('assets/', include('analytics.urls')),
    path('accounts/', include('django.contrib.auth.urls')),
    path('o/', include('oauth2_provider.urls', namespace='oauth2_provider')),
    path('auth/', include('accounts.urls')),
    path('api/token', TokenObtainPairView.as_view()),
    path('api/token/refresh', TokenRefreshView.as_view()),
    path('api/', include(router.urls))
]
