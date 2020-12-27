from rest_framework.exceptions import ValidationError
from rest_framework.fields import EmailField, CharField, JSONField
from rest_framework.serializers import Serializer, ModelSerializer

from accounts.models import User, GoogleIdentity


def validate_integration(value):
    integration_type = value['type']
    if integration_type == 'google':
        google_id = value['id']
        if not isinstance(google_id, str):
            raise ValidationError("'google' integration must provide id")
        if GoogleIdentity.objects.filter(google_id=google_id).exists():
            raise ValidationError("Google account is already registered to a user")
    else:
        raise ValidationError('Invalid integration type')


class CreateUserForm(Serializer):
    email = EmailField()
    username = CharField(max_length=64)

    authentication = JSONField(validators=[validate_integration])

    def create(self, validated_data):
        authentication = validated_data['authentication']
        user = User(username=validated_data['username'], email=validated_data['email'])
        user.set_unusable_password()
        user.save()

        if authentication['type'] == 'google':
            identity: GoogleIdentity = GoogleIdentity.objects.get(google_id=authentication['id'])
            identity.user = user
            identity.save()
        return user

    def update(self, instance, validated_data):
        pass


class UserSerializer(ModelSerializer):
    class Meta:
        model = User
        fields = ['username', 'uuid']
