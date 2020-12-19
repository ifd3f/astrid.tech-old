from rest_framework.exceptions import ValidationError
from rest_framework.fields import EmailField, CharField, JSONField
from rest_framework.serializers import Serializer

from accounts.models import User, GoogleIdentity


def validate_integration(value):
    if value['type'] == 'google':
        if not isinstance(value['id'], str):
            raise ValidationError("'google' integration must provide id")
    else:
        raise ValidationError('Invalid integration type')


class CreateUserForm(Serializer):
    email = EmailField()
    username = CharField(max_length=64)

    integration = JSONField(validators=[validate_integration])

    def create(self, validated_data):
        integration = validated_data['integration']
        user = User(username=validated_data['username'], email=validated_data['email'])
        user.set_unusable_password()
        user.save()

        if integration['type'] == 'google':
            identity: GoogleIdentity = GoogleIdentity.objects.get(google_id=integration['id'])
            identity.user = user
            identity.save()
        return user

    def update(self, instance, validated_data):
        pass



