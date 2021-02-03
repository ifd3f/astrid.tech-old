from django.urls import path

from printer3d.views import get_printer_image

urlpatterns = [
    path('<pk>/snapshot.jpg', get_printer_image)
]
