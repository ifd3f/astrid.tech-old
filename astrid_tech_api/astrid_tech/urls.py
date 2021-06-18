from django.conf import settings
from django.conf.urls.static import static
from django.contrib import admin
from django.urls import path, include
from rest_framework.routers import DefaultRouter
from rest_framework_simplejwt.views import TokenObtainPairView, TokenRefreshView

from blog.views import micropub
from comments.views import CommentViewSet
from printer3d.views import PrinterViewSet

router = DefaultRouter(trailing_slash=True)
router.register(r'comments', CommentViewSet, basename='comments')
router.register(r'3dprinter', PrinterViewSet, basename='3dprinter')

urlpatterns = [
                  path('admin/', admin.site.urls),
                  path('assets/', include('analytics.urls')),
                  path('accounts/', include('django.contrib.auth.urls')),
                  path('o/', include('oauth2_provider.urls', namespace='oauth2_provider')),
                  path('auth/', include('accounts.urls')),
                  path('api/micropub/', micropub),
                  path('api/token', TokenObtainPairView.as_view()),
                  path('api/token/refresh', TokenRefreshView.as_view()),
                  path('3dprinter/', include('printer3d.urls')),
                  path('api/', include(router.urls)),
              ] + static(settings.MEDIA_URL, document_root=settings.MEDIA_ROOT)
