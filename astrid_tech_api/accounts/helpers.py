from uuid import uuid4

from accounts.models import User, UserProfile


def create_user_with_profile(**user_args):
    user = User.objects.create_user(
        username=str(uuid4()),
        **user_args
    )
    user.set_unusable_password()
    user.save()

    profile = UserProfile.objects.create(user)
