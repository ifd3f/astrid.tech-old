from rest_framework.test import APITestCase

from blog.serializer import MicropubEntrySerializer


class MicropubSerializerTests(APITestCase):
    def test_micropub(self):
        form_data = {'h': 'entry', 'content': 'Hello World'}

        serializer = MicropubEntrySerializer(data=form_data)
        self.assertTrue(serializer.is_valid(raise_exception=True))
        entry = MicropubEntrySerializer.create_entry(serializer.validated_data)

        self.assertEqual('Hello World', entry.content)
