from django.test import TestCase

from analytics.models import Resource, NamedTracker, Hit


class TestNamedTracker(TestCase):
    def setUp(self) -> None:
        self.resource = Resource.objects.create(file='image.jpg', name='image.jpg')

    def test_returns_display_name_if_exists(self):
        NamedTracker.objects.create(file=self.resource, track_id='track69', display_name='display name')

        self.assertEqual('display name', NamedTracker.get_name(self.resource, 'track69'))

    def test_returns_generated_if_not_exists(self):
        self.assertEqual('image.jpg?t=track69', NamedTracker.get_name(self.resource, 'track69'))


class TestHit(TestCase):
    def setUp(self) -> None:
        self.resource = Resource.objects.create(file='image.jpg', name='image.jpg')

    def test_str_works(self):
        sut = Hit.objects.create(file=self.resource, track_id='420')

        self.assertTrue('Hit: image.jpg?t=420 @ ', str(sut))

    def test_str_works_with_named(self):
        sut = Hit.objects.create(file=self.resource, track_id='418')
        NamedTracker.objects.create(file=self.resource, track_id='418', display_name='im a teapot')

        self.assertIn('Hit: im a teapot @ ', str(sut))
