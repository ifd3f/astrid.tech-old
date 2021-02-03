import json
from datetime import datetime
from pathlib import Path

from django.contrib.auth import get_user_model
from django.contrib.auth.models import Permission
from rest_framework.test import APITestCase

from printer3d.models import Printer

TEST_IMG = Path(__file__).parent / "img.jpg"


class PrinterTestCase(APITestCase):
    def setUp(self):
        self.printer = Printer.objects.create(
            name="Example Printer",
            send_enabled=True
        )

        self.disallowed_user = get_user_model().objects.create_user(username='foobar', password='7812')
        self.allowed_user = get_user_model().objects.create_user(username='testuser', password='12345')
        self.allowed_user.user_permissions.add(Permission.objects.get(codename='change_printer'))

    def tearDown(self) -> None:
        self.printer.delete()

    def upload_image(self):
        with TEST_IMG.open('rb') as file:
            response = self.client.patch(
                f"/api/3dprinter/{self.printer.pk}/",
                {"image": file},
                format="multipart"
            )
        return response

    def test_can_list_printers(self):
        response = self.client.get(
            f"/api/3dprinter/"
        )
        self.assertEqual(response.status_code, 200)

    def test_anon_cannot_upload_image(self):
        response = self.upload_image()

        self.assertEqual(response.status_code, 401)

    def test_disallowed_cannot_upload_image(self):
        self.client.force_authenticate(user=self.disallowed_user)

        response = self.upload_image()

        self.assertEqual(response.status_code, 403)

    def test_authenticated_can_upload_image(self):
        self.client.force_authenticate(user=self.allowed_user)

        response = self.upload_image()

        self.assertEqual(response.status_code, 200)
        printer = Printer.objects.get(pk=self.printer.pk)
        self.assertIsNotNone(printer.image)

    def test_image_upload_updates_date(self):
        self.printer.last_updated = datetime.utcfromtimestamp(0)
        self.printer.save()
        self.client.force_authenticate(user=self.allowed_user)

        self.upload_image()

        printer = Printer.objects.get(pk=self.printer.pk)
        self.assertLess(printer.last_updated.timestamp() - datetime.utcnow().timestamp(), 10)

    def test_endpoint_updates_timestamp(self):
        self.printer.last_updated = datetime.utcfromtimestamp(0)
        self.printer.save()
        self.client.force_authenticate(user=self.allowed_user)
        self.upload_image()

        response = self.client.get(f"/api/3dprinter/{self.printer.pk}/")

        data = json.loads(str(response.content, encoding='utf-8'))
        self.assertNotEqual(data['last_updated'][:3], '1970')
