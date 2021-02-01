from datetime import datetime, timedelta
from pathlib import Path

from django.test import TestCase
from django.test.client import encode_multipart
from rest_framework.test import APITestCase

from printer3d.models import Printer

TEST_IMG = Path(__file__).parent / "img.jpg"


class PrinterTestCase(APITestCase):
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
        with TEST_IMG.open('rb') as file:
            response = self.client.patch(
                f"/api/3dprinter/{self.printer.pk}",
                {"image": file},
                format="multipart"
            )

        self.assertEqual(response.status_code, 200)
        printer = Printer.objects.get(pk=self.printer.pk)
        self.assertIsNotNone(printer.image)

