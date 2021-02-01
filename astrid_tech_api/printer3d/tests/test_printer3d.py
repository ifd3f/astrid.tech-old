from django.test import TestCase
from pathlib import Path

from rest_framework.test import APIRequestFactory

from printer3d.models import Printer
from printer3d.views import PrinterViewSet

TEST_IMG = Path(__file__).parent / "img.png"


class PrinterTestCase(TestCase):
    def setUp(self):
        self.printer = Printer.objects.create(
            name="Example Printer",
            send_enabled=True
        )

    def tearDown(self) -> None:
        self.printer.delete()

    def test_can_list_printers(self):
        response = self.client.get(
            f"/api/3dprinter/"
        )
        self.assertEqual(response.status_code, 200)

    def test_can_upload_image(self):
        with TEST_IMG.open() as file:
            response = self.client.post(
                f"/api/3dprinter/{self.printer.pk}/image/",
                {'file': file},
                content_type='image/png'
            )
        self.assertEqual(response.status_code, 200)
