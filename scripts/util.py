import os

from requests import Session


def create_auth_session() -> Session:
    token = os.getenv('API_TOKEN')
    s = Session()
    s.headers.update({'Authorization': f'Bearer {token}'})

    return s
