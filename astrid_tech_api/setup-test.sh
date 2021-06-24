echo "from django.contrib.auth import get_user_model; User = get_user_model(); User.objects.create_superuser('admin', 'admin@myproject.com', 'password')" | python manage.py shell
echo "Created superuser 'admin' with password 'password'"

openssl genrsa -out oidc.key 4096